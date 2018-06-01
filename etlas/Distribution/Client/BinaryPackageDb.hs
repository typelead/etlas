{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Distribution.Client.BinaryPackageDb where

import Distribution.Client.GlobalFlags
import Distribution.Client.HttpUtils
import Distribution.Client.Types

import Distribution.Simple.Utils
import Distribution.Simple.Setup ( fromFlag )
import Distribution.System
import Distribution.Verbosity
import Distribution.Version
import Distribution.Text
import Distribution.Types.PackageId

import Network.URI
import System.Directory
import System.FilePath

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
    _         -> Nothing

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

commitHashPath :: Either String Version -> FilePath
commitHashPath version = eEtaVersion version </> "commit-hash"

packageIndexPath :: Either String Version -> FilePath
packageIndexPath version = eEtaVersion version </> "packages" </> "index"

basePackageIndexPath :: Either String Version -> FilePath
basePackageIndexPath version = eEtaVersion version </> "packages" </> "base-index"

etaBinariesIndexPath :: Either String Version -> FilePath
etaBinariesIndexPath version =
  eEtaVersion version </> "binaries" </> display buildPlatform </> "index"

etaProgPath :: String -> Either String Version -> FilePath
etaProgPath prog version =
  eEtaVersion version </> "binaries" </> display buildPlatform </> (prog ++ ext)
  where ext
          | Platform _ Windows <- buildPlatform = ".exe"
          | otherwise = ""

topLevelIndexPath :: String
topLevelIndexPath = "index"

-- Converts a local path to a normalised URI path
uriWithPath :: URI -> FilePath -> URI
uriWithPath uri path = uri { uriPath = joinBase $ map makeUniform path }
  where makeUniform c = if c == pathSeparator then '/' else c
        oldPath = uriPath uri
        joinBase relPath
          | not (null oldPath) && last oldPath == '/'
          = oldPath ++ relPath
          | otherwise = oldPath ++ ('/':relPath)

-- Local Paths

topLevelIndexFile :: FilePath -> URIDomain -> FilePath
topLevelIndexFile binariesPath uriName =
  binariesPath </> uriName </> topLevelIndexPath

commitHashFile :: FilePath -> URIDomain -> Either String Version -> FilePath
commitHashFile binariesPath uriName version =
  binariesPath </> uriName </> eEtaVersion version </> "commit-hash"

packageIndexFile :: FilePath -> URIDomain -> Either String Version -> FilePath
packageIndexFile binariesPath uriName version =
  binariesPath </> uriName </> packageIndexPath version

basePackageIndexFile :: FilePath -> URIDomain -> Either String Version -> FilePath
basePackageIndexFile binariesPath uriName version =
  binariesPath </> uriName </> basePackageIndexPath version

etaBinariesIndexFile :: FilePath -> URIDomain -> Either String Version -> FilePath
etaBinariesIndexFile binariesPath uriName version =
  binariesPath </> uriName </> etaBinariesIndexPath version

etaProgFile :: FilePath -> URIDomain -> String -> Either String Version -> FilePath
etaProgFile binariesPath uriName prog version =
  binariesPath </> uriName </> etaProgPath prog version

etaInstalledFile :: FilePath -> URIDomain -> String -> FilePath
etaInstalledFile binariesPath uriName version =
  binariesPath </> uriName </> version </> "installed"

readBinaryIndexFile :: (PackageName -> URI) -> FilePath -> IO BinaryPackageDb
readBinaryIndexFile mkUri indexFilePath =
  foldl' (\m line ->
            case simpleParse line of
              Just pkgId -> insertBinaryPackageDb pkgId [mkUri line] m
              Nothing    -> m)
    mempty `fmap` readLines indexFilePath

etaVersionedPath :: Version -> String
etaVersionedPath version = "eta-" ++ display version

eEtaVersion :: Either String Version -> String
eEtaVersion = either id etaVersionedPath

fetchBinaryPackageDb :: Verbosity -> RepoContext -> FilePath -> Version -> URI
                     -> IO BinaryPackageDb
fetchBinaryPackageDb verbosity repoCtxt binariesPath version uri
  | Just uriName <- getURIDomain uri
  , let indexFile = packageIndexFile binariesPath uriName (Right version)
        genMkEntry pkgName = uriWithPath uri $
          etaVersionedPath version </> "packages" </> (pkgName ++ "-bin.tar.gz")
  = fmap (fromMaybe mempty) $
      ifTrue (ifConditionElseRetry
                (doesFileExist indexFile)
                (do transport <- repoContextGetTransport repoCtxt
                    checkAndDownloadVersionIndex verbosity transport binariesPath
                      uriName uri (Right version))) $
        fmap Just $ readBinaryIndexFile genMkEntry indexFile
  | otherwise = do
      info verbosity $ "The url " ++ show uri ++ " is not valid."
      return mempty

getBinaryPackages :: Verbosity -> RepoContext -> FilePath -> Maybe Version
                  -> IO BinaryPackageDb
getBinaryPackages verbosity repoCtxt binariesPath etaVersion' = do
  when (isNothing etaVersion') $
    die' verbosity "Unable to determine eta version."

  let etaVersion = fromJust etaVersion'

  fmap (mconcat . concat) $
    forM (gitIndexedRepos repoCtxt) $ \repo -> do
      uris <- remoteBinaryUris (Right repo)
      mapM (fetchBinaryPackageDb verbosity repoCtxt binariesPath etaVersion) uris

cachedBinaryPackagePath :: FilePath -> URI -> FilePath
cachedBinaryPackagePath binariesPath uri =
  binariesPath </> (fromJust $ getURIDomain uri) ++ uriPath uri

tryDownloadBinary :: Verbosity -> HttpTransport -> FilePath -> BinaryPackageDb
                  -> PackageId -> IO (Maybe FilePath)
tryDownloadBinary verbosity transport binariesPath binaryPkgDb pkgid
  | Just (uri:_) <- lookupBinaryPackageDb pkgid binaryPkgDb
  = do let cached = cachedBinaryPackagePath binariesPath uri
       exists <- doesFileExist cached
       if not exists
       then do
         notice verbosity $ "Downloading [binary] " ++ display pkgid ++ "..."
         createDirectoryIfMissingVerbose verbosity True (takeDirectory cached)
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

checkAndDownloadVersionIndex :: Verbosity -> HttpTransport -> FilePath
                             -> URIDomain -> URI -> Either String Version -> IO ()
checkAndDownloadVersionIndex verbosity transport binariesPath domain uri eVersion =
  fmap (fromMaybe ()) $
    ifTrue (doesFileExist indexFile) $
      ifTrue ((any (== (eEtaVersion eVersion))) `fmap` readLines indexFile) $
        fmap Just $ downloadVersionIndex verbosity transport binariesPath
                      domain uri eVersion
  where indexFile = topLevelIndexFile binariesPath domain

downloadVersionIndex :: Verbosity -> HttpTransport -> FilePath -> URIDomain -> URI
                     -> Either String Version -> IO ()
downloadVersionIndex verbosity transport binariesPath domain uri eVersion = do
  let pkgIdxFile         = packageIndexFile binariesPath domain eVersion
      basePkgIdxFile     = basePackageIndexFile binariesPath domain eVersion
      etaBinaryIdxFile   = etaBinariesIndexFile binariesPath domain eVersion
      msgWithVersion msg = "[" ++ userReadableVersion (eEtaVersion eVersion)
                        ++ "] Unable to download " ++ msg
  createDirectoryIfMissingVerbose verbosity True (takeDirectory pkgIdxFile)
  createDirectoryIfMissingVerbose verbosity True (takeDirectory basePkgIdxFile)
  createDirectoryIfMissingVerbose verbosity True (takeDirectory etaBinaryIdxFile)
  downloadURIWithMsg (msgWithVersion "package index file") transport
    verbosity (uriWithPath uri (packageIndexPath eVersion)) pkgIdxFile
  downloadURIWithMsg (msgWithVersion "base package index file") transport
    verbosity (uriWithPath uri (basePackageIndexPath eVersion)) basePkgIdxFile
  downloadURIWithMsg (msgWithVersion "binary index file") transport
    verbosity (uriWithPath uri (etaBinariesIndexPath eVersion)) etaBinaryIdxFile
  return ()

ifConditionElseRetry :: (Monad m) => m Bool -> m () -> m Bool
ifConditionElseRetry mPred mEffect = do
  bool <- mPred
  if bool then return True
  else mEffect >> mPred

userReadableVersion :: String -> String
userReadableVersion version = reverse ('b' : drop 1 rest) ++ reverse buildNumber
  where (buildNumber, rest) = break (== '.') $ reverse version

downloadURIWithMsg :: String -> HttpTransport -> Verbosity -> URI -> FilePath -> IO ()
downloadURIWithMsg msg transport verbosity uri path =
  void $ downloadURIAllowFail handler transport verbosity uri path
  where handler _ = info verbosity $ msg ++ " - " ++ uriToString id uri ""

ifTrue :: Monad m => m Bool -> m (Maybe a) -> m (Maybe a)
ifTrue mb action = do
  b <- mb
  if b then action else return Nothing

binariesDirFrom :: GlobalFlags -> FilePath
binariesDirFrom globalFlags = fromFlag $ globalBinariesDir globalFlags
