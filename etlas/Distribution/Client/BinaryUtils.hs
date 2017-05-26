{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Distribution.Client.BinaryUtils (
    updateBinaryPackageCaches
  , findEtaInBinaryIndex
  ) where

import Distribution.Client.BinaryPackageDb
import Distribution.Client.IndexUtils

import Distribution.Client.Config
import Distribution.Client.GlobalFlags
import Distribution.Client.HttpUtils
import {-# SOURCE #-} Distribution.Client.Install
import {-# SOURCE #-} Distribution.Client.Sandbox
import Distribution.Client.Setup as Setup
import Distribution.Client.Sandbox.Types
import Distribution.Client.Targets

import Distribution.Solver.Types.Settings

import Distribution.Simple.Command
import Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Setup hiding (GlobalFlags)
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version
import Distribution.Text

import Network.URI
import System.Directory
import System.FilePath

import Control.Monad
import Data.List
import Data.Maybe

updateBinaryPackageCaches :: HttpTransport -> Verbosity -> FilePath -> IO ()
updateBinaryPackageCaches transport verbosity cacheDir = do
  notice verbosity $ "Updating binary package index..."
  uris <- remoteBinaryUris (Left cacheDir)
  forM_ uris $ \uri -> do
    case getURIDomain uri of
      Just domain -> do
        let indexFile = topLevelIndexFile domain
        createDirectoryIfMissingVerbose verbosity True (takeDirectory indexFile)
        _ <- downloadURIWithMsg "Failed to download top-level index file."
               transport verbosity (uriWithPath uri topLevelIndexPath) indexFile
        versions <- readLines indexFile
        forM_ versions $ \version -> do

          let pkgIdxFile     = packageIndexFile     domain (Left version)
              basePkgIdxFile = basePackageIndexFile domain (Left version)
              etaBinaryIdxFile  = etaBinariesIndexFile domain (Left version)
              msgWithVersion msg = "[" ++ userReadableVersion version
                                ++ "] Failed to download " ++ msg
          createDirectoryIfMissingVerbose verbosity True (takeDirectory pkgIdxFile)
          createDirectoryIfMissingVerbose verbosity True (takeDirectory basePkgIdxFile)
          createDirectoryIfMissingVerbose verbosity True (takeDirectory etaBinaryIdxFile)
          _ <- downloadURIWithMsg (msgWithVersion "package index file") transport
                 verbosity (uriWithPath uri (packageIndexPath (Left version)))
                 pkgIdxFile
          _ <- downloadURIWithMsg (msgWithVersion "base package index file") transport
                 verbosity (uriWithPath uri (basePackageIndexPath (Left version)))
                 basePkgIdxFile
          _ <- downloadURIWithMsg (msgWithVersion "binary index file") transport
                 verbosity (uriWithPath uri (etaBinariesIndexPath (Left version)))
                 etaBinaryIdxFile
          return ()

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
    dieWithError verbosity "Your binary cache seems to be empty."

  exists <- doesFileExist indexFile

  when (not exists) $
    dieWithError verbosity "Your binary cache index file seems to be missing."

  packageLines <- readLines indexFile
  forM packageLines $ \line -> do
    case simpleParse line of
      Just pkgId -> do
        result <- tryDownloadBinary verbosity transport binaryPkgDb pkgId
        case result of
          Just path -> return path
          Nothing   -> dieWithError verbosity $ "Unable to find base package "
                                             ++ display pkgId ++ " in binary package index."
      Nothing -> dieWithError verbosity "Your binary cache index file seems to corrupted."

etaPointerFile :: FilePath
etaPointerFile = defaultBinariesPath </> "eta"

-- The constant that denotes that you should use the `eta` that's on the path.
programOnPath :: String
programOnPath = "$PATH"

-- NOTE: When you add a new program to the eta ecosystem, you MUST increment this
--       number and list it below:
--       1. eta
--       2. eta-pkg
numEtaPrograms :: Int
numEtaPrograms = 2

-- TODO: This is not thread-safe. Address if this becomes a problem.
findEtaInBinaryIndex :: String -> Int -> GlobalFlags -> Verbosity -> ProgramSearchPath -> IO (Maybe (FilePath, [FilePath]))
findEtaInBinaryIndex prog n globalFlags' verbosity searchPath = do
  savedConfig <- fmap snd $ loadConfigOrSandboxConfig verbosity globalFlags'
  let globalFlags = savedGlobalFlags savedConfig `mappend` globalFlags'
  -- TODO: Synchronize access to this file
  exists <- doesFileExist etaPointerFile
  if exists
  then do
    pointerLines <- readLines etaPointerFile
    case nth n pointerLines of
      Just progPath
        | progPath == programOnPath -> do
          result <- defaultCodePath
          if isNothing result
          then initEtaPointer globalFlags savedConfig
          else return result
        | otherwise                 -> return $ Just (progPath, [])
      _     -> initEtaPointer globalFlags savedConfig
  else initEtaPointer globalFlags savedConfig
  where initEtaPointer globalFlags savedConfig = do
          notice verbosity $
            "Discovering the installation paths for your Eta executables..."
          result <- defaultCodePath
          case result of
            Just (path, _paths) -> do
              notice verbosity $
                "Found installed '" ++ prog ++ "' at " ++ path ++ "."
              writeFile etaPointerFile $ unlines
                                       $ replicate numEtaPrograms programOnPath
              return result
            Nothing -> do
              notice verbosity $
                  "No existing installation found for '" ++ prog ++ "'.\n"
               ++ "Attempting to download binaries..."
              result <- searchRepos globalFlags savedConfig
              if isNothing result
              then do
                _ <- dieWithError verbosity $
                        "Unable to find an Eta binary for your platform.\n"
                     ++ "Either install from source or try the following:"
                return Nothing
              else return result
        searchRepos globalFlags savedConfig =
          withRepoContext verbosity globalFlags $ \repoCtxt -> do
            first (gitIndexedRepos repoCtxt) $ \repo -> do
              uris <- remoteBinaryUris (Right repo)
              first uris $ \uri -> do
                case getURIDomain uri of
                  Just domain -> do
                    let indexFile = topLevelIndexFile domain
                    exists <- doesFileExist indexFile
                    ifTrue exists $ do
                      versions <- readLines indexFile
                      first (reverse versions) $ \version -> do
                        let binaryIndexFile = etaBinariesIndexFile domain (Left version)
                        exists <- doesFileExist binaryIndexFile
                        ifTrue exists $ do
                          programs <- readLines binaryIndexFile
                          programPaths <- downloadPrograms repoCtxt domain uri version programs
                          notice verbosity $
                            "Selected " ++ userReadableVersion version ++ "."
                          installBootLibraries verbosity (readVersion version)
                            repoCtxt savedConfig globalFlags programPaths
                          writeFile etaPointerFile $ unlines programPaths
                          case nth n programPaths of
                            -- TODO: Improve this for change monitoring
                            Just path -> do
                              return $ Just (path, [])
                            _         -> return Nothing
                  Nothing -> dieWithError verbosity "Bad uri in index file."
        downloadPrograms repoCtxt domain uri version programs = do
          transport <- repoContextGetTransport repoCtxt
          forM programs $ \prog -> do
            notice verbosity $ "Downloading executable '" ++ prog ++ "'..."
            let progFile = etaProgFile domain prog (Left version)
            _ <- downloadURI transport verbosity
                   (uriWithPath uri (etaProgPath prog (Left version))) progFile
            setFileExecutable progFile
            return progFile
        defaultCodePath = findProgramOnSearchPath verbosity searchPath prog

ifTrue :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
ifTrue b action
  | b         = action
  | otherwise = return Nothing

first :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
first []     _ = return Nothing
first (x:xs) f = do
  m <- f x
  if isJust m
  then return m
  else first xs f

nth :: Int -> [a] -> Maybe a
nth 0 (x:_)  = Just x
nth n (_:xs) = nth (n - 1) xs
nth _ _      = Nothing

dieWithError :: Verbosity -> String -> IO a
dieWithError verbosity msg = die' verbosity $
                             msg ++ "\n"
                           ++ "Run `etlas update` and try again.\n"
                           ++ "If that doesn't work, please report this as a bug at\n\n"
                           ++ "https://github.com/typelead/eta/issues/new\n\n"
                           ++ "specifying your Etlas version."

downloadURIWithMsg :: String -> HttpTransport -> Verbosity -> URI -> FilePath -> IO (Maybe ())
downloadURIWithMsg msg transport verbosity uri path =
  downloadURIAllowFail handler transport verbosity uri path
  where handler _ = putStrLn $ msg ++ " - " ++ uriToString id uri ""

userReadableVersion :: String -> String
userReadableVersion version = reverse ('b' : drop 1 rest) ++ reverse buildNumber
  where (buildNumber, rest) = break (== '.') $ reverse version

readVersion :: String -> Maybe Version
readVersion versionString = do
  version <- stripPrefix "eta-" versionString
  simpleParse version

installBootLibraries
  :: Verbosity
  -> Maybe Version
  -> RepoContext
  -> SavedConfig
  -> GlobalFlags
  -> [FilePath]
  -> IO ()
installBootLibraries verbosity mVersion repos config globalFlags programPaths = do
  transport <- repoContextGetTransport repos
  dist <- findSavedDistPref config mempty
  binaryPkgDb <- getBinaryPackages verbosity repos mVersion
  paths <- getBasePackageBinaryPaths verbosity transport binaryPkgDb mVersion
  let (configFlags', configExFlags', installFlags', haddockFlags')
        = commandDefaultFlags Setup.installCommand
      configFlags = savedConfigureFlags config `mappend`
                    configFlags' { configDistPref = toFlag dist
                                 , configHcPath   = maybeToFlag $ nth 0 programPaths
                                 , configHcPkg    = maybeToFlag $ nth 1 programPaths }

      configExFlags  = defaultConfigExFlags         `mappend`
                         savedConfigureExFlags config `mappend` configExFlags'
      installFlags   = Setup.defaultInstallFlags      `mappend`
                         savedInstallFlags     config `mappend` installFlags' {
                           installAllowBootLibInstalls =
                             toFlag (AllowBootLibInstalls True),
                           installOverrideReinstall = toFlag True
                         }
      haddockFlags   = defaultHaddockFlags          `mappend`
                         savedHaddockFlags     config `mappend`
                         haddockFlags' { haddockDistPref = toFlag dist }
      packageDBs = configPackageDB' configFlags

  (comp, platform, progdb') <- configCompilerAux' configFlags
  progdb <- configureAllKnownPrograms verbosity progdb'
  installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
  if missingBootLibraries installedPkgIndex
  then do
    notice verbosity "Installing boot libraries..."
    forM_ paths $ \path -> do
      install verbosity packageDBs repos comp platform progdb NoSandbox
        Nothing globalFlags configFlags configExFlags installFlags
        haddockFlags [UserTargetLocalTarball path True]
  else notice verbosity "Boot libraries already installed."


missingBootLibraries :: InstalledPackageIndex -> Bool
missingBootLibraries pkgIdx = not . all
                              (\pkg ->
                                case PackageIndex.searchByName pkgIdx pkg of
                                  PackageIndex.None -> False
                                  _                 -> True)
                            $ bootPackages
  where bootPackages = ["rts", "base", "ghc-prim", "integer", "template-haskell"]
