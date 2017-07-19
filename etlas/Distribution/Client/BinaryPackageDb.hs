{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Distribution.Client.BinaryPackageDb where

import Distribution.Client.Config
import Distribution.Client.GlobalFlags
import Distribution.Client.HttpUtils
import Distribution.Client.Types

import Distribution.Simple.Utils
import Distribution.System
import Distribution.Verbosity
import Distribution.Version
import Distribution.Text
import Distribution.Types.PackageId

import Network.URI
import System.Directory
import System.FilePath
import System.IO.Unsafe

import Control.Monad
import Control.Exception
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

-- URI Paths

packageIndexPath :: Either String Version -> FilePath
packageIndexPath version = either id etaVersionedPath version
                        </> "packages" </> "index"

basePackageIndexPath :: Either String Version -> FilePath
basePackageIndexPath version = either id etaVersionedPath version
                            </> "packages" </> "base-index"

etaBinariesIndexPath :: Either String Version -> FilePath
etaBinariesIndexPath version = either id etaVersionedPath version
                            </> "binaries" </> ("index." ++ display buildPlatform)

etaProgPath :: String -> Either String Version -> FilePath
etaProgPath prog version = either id etaVersionedPath version
                        </> "binaries" </> (prog ++ "_" ++ display buildPlatform)

topLevelIndexPath :: String
topLevelIndexPath = "index"

-- Converts a local path to a normalised URI path
uriWithPath :: URI -> FilePath -> URI
uriWithPath uri path = uri { uriPath = "/" ++ map makeUniform path }
  where makeUniform c = if c == pathSeparator then '/' else c

-- Local Paths

defaultBinariesPath :: FilePath
defaultBinariesPath = unsafePerformIO $ do
  dir <- defaultCabalDir
  return $ dir </> "binaries"

topLevelIndexFile :: URIDomain -> FilePath
topLevelIndexFile uriName = defaultBinariesPath </> uriName </> topLevelIndexPath

packageIndexFile :: URIDomain -> Either String Version -> FilePath
packageIndexFile uriName version
  = defaultBinariesPath </> uriName </> packageIndexPath version

basePackageIndexFile :: URIDomain -> Either String Version -> FilePath
basePackageIndexFile uriName version
  = defaultBinariesPath </> uriName </> basePackageIndexPath version

etaBinariesIndexFile :: URIDomain -> Either String Version -> FilePath
etaBinariesIndexFile uriName version
  = defaultBinariesPath </> uriName </> etaBinariesIndexPath version

etaProgFile :: URIDomain -> String -> Either String Version -> FilePath
etaProgFile uriName prog version
  = defaultBinariesPath </> uriName </> etaProgPath prog version

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
        genMkEntry pkgName = uriWithPath uri $
          etaVersionedPath version </> "packages" </> (pkgName ++ "-bin.tar.gz")
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
       if not exists
       then do
         notice verbosity $ "Downloading [binary] " ++ display pkgid ++ "..."
         result <- downloadURIAllowFail (const $ return True) transport
                     verbosity uri cached
         return $ maybe (Just cached) (const Nothing) result
       else return (Just cached)
  | otherwise
  = return Nothing

downloadURIAllowFail :: (SomeException -> IO a) -> HttpTransport -> Verbosity -> URI -> FilePath -> IO (Maybe a)
downloadURIAllowFail handler transport verbosity uri path =
  catch (downloadURI transport (lessVerbose verbosity) uri path >> return Nothing)
        (fmap Just . handler)

