{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Distribution.Client.BinaryPackageDb (
    BinaryPackageDb
  , lookupBinaryPackageDb
  , insertBinaryPackageDb

  , getBinaryPackages
  , tryDownloadBinary
  , updateBinaryPackageCaches
  , getBasePackageBinaryPaths
  ) where

import {-# SOURCE #-} Distribution.Client.Config
import Distribution.Client.HttpUtils
import Distribution.Client.GlobalFlags
import Distribution.Client.Types
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version
import Distribution.Text
import Distribution.Types.PackageId

import Network.URI
import System.Directory
import System.FilePath
import System.IO.Unsafe

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M

type GitIndexedRepo = Repo
type URIDomain = String
type PackageName = String

newtype BinaryPackageDb = BinaryPackageDb (M.Map PackageId [URI])

instance Monoid BinaryPackageDb where

  BinaryPackageDb x `mappend` BinaryPackageDb y =

    BinaryPackageDb $ M.unionWith (++) x y

  mempty = BinaryPackageDb mempty

lookupBinaryPackageDb :: PackageId -> BinaryPackageDb -> Maybe [URI]
lookupBinaryPackageDb pkgid (BinaryPackageDb db) = M.lookup pkgid db

insertBinaryPackageDb :: PackageId -> [URI] -> BinaryPackageDb
                      -> BinaryPackageDb
insertBinaryPackageDb pkgid uris (BinaryPackageDb db) =
  BinaryPackageDb $ M.insert pkgid uris db

getArbitraryURI :: BinaryPackageDb -> Maybe URI
getArbitraryURI (BinaryPackageDb db) =
  case M.elems db of
    (uri:_):_ -> Just uri
    _     -> Nothing

gitIndexedRepos :: RepoContext -> [GitIndexedRepo]
gitIndexedRepos repoCtxt = filter ((== Just True)
                                  . fmap remoteRepoGitIndexed
                                  . maybeRepoRemote)
                         $ repoContextRepos repoCtxt

binaryDir :: FilePath
binaryDir = "binaries"

binaryMirrorUrlsFile :: FilePath
binaryMirrorUrlsFile = binaryDir </> "urls"

getURIDomain :: URI -> Maybe String
getURIDomain = fmap uriRegName . uriAuthority

remoteBinaryUris :: Either FilePath GitIndexedRepo -> IO [URI]
remoteBinaryUris cacheDirOrRepo = do
  let mirrorsList = either id repoLocalDir cacheDirOrRepo
                </> binaryMirrorUrlsFile
  exists <- doesFileExist mirrorsList
  if exists
  then return . catMaybes . map parseURI =<< readLines mirrorsList
  else return []

readLines :: FilePath -> IO [String]
readLines path = readFile path >>= return . filter (not . all isSpace) . lines

packageIndexPath :: Either String Version -> FilePath
packageIndexPath version = either id etaVersionedPath version
                        </> "packages" </> "index"

basePackageIndexPath :: Either String Version -> FilePath
basePackageIndexPath version = either id etaVersionedPath version
                            </> "packages" </> "base-index"

topLevelIndexPath :: String
topLevelIndexPath = "index"

topLevelIndexFile :: URIDomain -> FilePath
topLevelIndexFile uriName = defaultBinariesPath </> uriName </> topLevelIndexPath

packageIndexFile :: URIDomain -> Either String Version -> FilePath
packageIndexFile uriName version
  = defaultBinariesPath </> uriName </> packageIndexPath version

basePackageIndexFile :: URIDomain -> Either String Version -> FilePath
basePackageIndexFile uriName version
  = defaultBinariesPath </> uriName </> basePackageIndexPath version

readBinaryIndexFile :: (PackageName -> URI) -> FilePath -> IO BinaryPackageDb
readBinaryIndexFile mkUri indexFilePath = do
  packageLines <- readLines indexFilePath
  return . foldl' (\m line -> case simpleParse line of
                                Just pkgId ->
                                  insertBinaryPackageDb pkgId [mkUri line] m
                                Nothing    -> m)
                  mempty
         $ packageLines

etaVersionedPath :: Version -> String
etaVersionedPath version = "eta-" ++ display version

fetchBinaryPackageDb :: Verbosity -> Version -> URI -> IO BinaryPackageDb
fetchBinaryPackageDb verbosity version uri
  | Just uriName <- getURIDomain uri
  , let indexFile = packageIndexFile uriName (Right version)
        genMkEntry pkgName = uri {
            uriPath = "/" ++ (etaVersionedPath version </> "packages" </> (pkgName ++ "-bin.tar.gz"))
          }
  = do exists <- doesFileExist indexFile
       if exists
       then readBinaryIndexFile genMkEntry indexFile
       else return mempty
  | otherwise = do
      info verbosity $ "The url " ++ show uri ++ " is not valid."
      return mempty

getBinaryPackages :: Verbosity -> RepoContext -> Maybe Version -> IO BinaryPackageDb
getBinaryPackages verbosity repoCtxt etaVersion' = do

  when (isNothing etaVersion') $
    die' verbosity "Unable to determine eta version."

  let etaVersion = fromJust etaVersion'

  binaryPackageDbs <- forM (gitIndexedRepos repoCtxt) $ \repo -> do
                        uris <- remoteBinaryUris (Right repo)
                        mapM (fetchBinaryPackageDb verbosity etaVersion) uris

  return $ mconcat $ concat $ binaryPackageDbs

cachedBinaryPackagePath :: URI -> FilePath
cachedBinaryPackagePath uri = defaultBinariesPath
                          </> (fromJust $ getURIDomain uri)
                           ++ uriPath uri

tryDownloadBinary :: Verbosity -> HttpTransport -> BinaryPackageDb -> PackageId
                  -> IO (Maybe FilePath)
tryDownloadBinary verbosity transport binaryPkgDb pkgid
  | Just (uri:_) <- lookupBinaryPackageDb pkgid binaryPkgDb
  = do let cached = cachedBinaryPackagePath uri
       exists <- doesFileExist cached
       when (not (exists)) $ do
         notice verbosity $ "Downloading [binary] " ++ display pkgid ++ "..."
         _ <- downloadURI transport verbosity uri cached
         return ()
       return (Just cached)
  | otherwise
  = return Nothing

defaultBinariesPath :: FilePath
defaultBinariesPath = unsafePerformIO $ do
  dir <- defaultCabalDir
  return $ dir </> "binaries"

updateBinaryPackageCaches :: HttpTransport -> Verbosity -> FilePath -> IO ()
updateBinaryPackageCaches transport verbosity cacheDir = do
  notice verbosity $ "Updating binary package index..."
  uris <- remoteBinaryUris (Left cacheDir)
  forM_ uris $ \uri -> do
    case getURIDomain uri of
      Just domain -> do
        let indexFile = topLevelIndexFile domain
        createDirectoryIfMissingVerbose verbosity True (takeDirectory indexFile)
        _ <- downloadURI transport verbosity uri { uriPath = "/" ++ topLevelIndexPath } indexFile
        versions <- readLines indexFile
        forM_ versions $ \version -> do
          let pkgIdxFile     = packageIndexFile     domain (Left version)
          let basePkgIdxFile = basePackageIndexFile domain (Left version)
          createDirectoryIfMissingVerbose verbosity True (takeDirectory pkgIdxFile)
          createDirectoryIfMissingVerbose verbosity True (takeDirectory basePkgIdxFile)
          _ <- downloadURI transport verbosity uri {
                  uriPath = "/" ++ packageIndexPath (Left version)
                } pkgIdxFile
          downloadURI transport verbosity uri {
                  uriPath = "/" ++ basePackageIndexPath (Left version)
                } basePkgIdxFile

      Nothing -> die' verbosity $ "Invalid domain name for URL: " ++ show uri

getBasePackageBinaryPaths :: Verbosity -> HttpTransport -> BinaryPackageDb
                          -> Maybe Version -> IO [FilePath]
getBasePackageBinaryPaths verbosity transport binaryPkgDb mVersion = do

  when (isNothing mVersion) $
    die' verbosity "getBasePackageBinaryPaths: Unable to determine eta version."

  let etaVersion = fromJust mVersion
      -- NOTE: We assume that base-index is the same across mirrors for a given eta version
      mUri       = getArbitraryURI binaryPkgDb
      domain     = fromJust (getURIDomain (fromJust mUri))
      indexFile  = basePackageIndexFile domain (Right etaVersion)

  when (isNothing mUri) $
    dieWithError "Your binary cache seems to be empty."

  exists <- doesFileExist indexFile

  when (not exists) $
    dieWithError "Your binary cache index file seems to be missing."

  packageLines <- readLines indexFile
  forM packageLines $ \line -> do
    case simpleParse line of
      Just pkgId -> do
        result <- tryDownloadBinary verbosity transport binaryPkgDb pkgId
        case result of
          Just path -> return path
          Nothing   -> dieWithError $ "Unable to find base package "
                                   ++ display pkgId ++ " in binary package index."
      Nothing -> dieWithError "Your binary cache index file seems to corrupted."

  where dieWithError msg = die' verbosity $
                            msg ++ "\n"
                         ++ "Run `etlas update` and try again.\n"
                         ++ "If that doesn't work, please report this as a bug at\n\n"
                         ++ "https://github.com/typelead/eta/issues/new\n\n"
                         ++ "specifying your Eta & Etlas versions."

