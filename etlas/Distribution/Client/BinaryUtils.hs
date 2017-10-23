{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Client.BinaryUtils (
    updateBinaryPackageCaches
  , findEtaInBinaryIndex
  , findCoursier
  , findVerify
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
import Distribution.Client.Update

import Distribution.Solver.Types.Settings

import Distribution.Simple.Command
import Distribution.Simple.InstallDirs
import Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Setup hiding (GlobalFlags)
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version
import Distribution.Text
import qualified Paths_etlas (version)

import Control.Exception
import Network.URI
import System.Directory
import System.FilePath

import Control.Monad
import Data.List
import Data.Maybe

updateBinaryPackageCaches :: HttpTransport -> Verbosity -> FilePath -> IO ()
updateBinaryPackageCaches transport verbosity cacheDir = do
  uris <- remoteBinaryUris (Left cacheDir)
  notice verbosity $ "Updating binary package index."
  forM_ uris $ \uri -> do
    case getURIDomain uri of
      Just domain -> do
        let indexFile = topLevelIndexFile domain
        createDirectoryIfMissingVerbose verbosity True (takeDirectory indexFile)
        _ <- downloadURIWithMsg "Unable to download top-level index file."
               transport verbosity (uriWithPath uri topLevelIndexPath) indexFile
        versions <- readLines indexFile
        forM_ versions $ \version -> do

          let pkgIdxFile         = packageIndexFile     domain (Left version)
              basePkgIdxFile     = basePackageIndexFile domain (Left version)
              etaBinaryIdxFile   = etaBinariesIndexFile domain (Left version)
              msgWithVersion msg = "[" ++ userReadableVersion version
                                ++ "] Unable to download " ++ msg
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
      etaVersion = flagToMaybe $ globalEtaVersion globalFlags
  case etaVersion of
    Nothing -> do
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
    Just version' -> do
      let version = "eta-" ++ map (\c -> if c == 'b' then '.' else c) version'
      mResult <- withRepoContext verbosity globalFlags $ \repoCtxt ->
                   selectVersion globalFlags repoCtxt savedConfig version
      case mResult of
        ret@(Just _) -> return ret
        Nothing -> do
          _ <- dieWithError verbosity $
                  "Unable to find " ++ version ++ " in any of your configured binary indices.\n"
          return Nothing
  where initEtaPointer globalFlags savedConfig = do
          notice verbosity $
            "Discovering the installation paths for your Eta executables..."
          result <- defaultCodePath
          case result of
            Just (path, _paths) -> do
              notice verbosity $
                "Found installed '" ++ prog ++ "' at " ++ path ++ "."
              createDirectoryIfMissingVerbose verbosity True (takeDirectory etaPointerFile)
              writeFile etaPointerFile $ unlines
                                       $ replicate numEtaPrograms programOnPath
              return result
            Nothing -> do
              notice verbosity $
                  "No existing installation found for '" ++ prog ++ "'.\n"
               ++ "Attempting to download binaries..."
              mResult <- withRepoContext verbosity globalFlags $ \repoCtxt ->
                           goSearchRepos globalFlags repoCtxt savedConfig

              case mResult of
                ret@(Just _) -> return ret
                Nothing -> do
                  _ <- dieWithError verbosity "Unable to find any Eta binaries for your platform."
                  return Nothing

        tryUpdateIfFailed globalFlags repoCtxt savedConfig f = do
          result <- f globalFlags repoCtxt savedConfig
          if isNothing result
          then do
            exists <- doesDirectoryExist defaultBinariesPath
            if exists
            then return Nothing
            else do
              update verbosity (commandDefaultFlags updateCommand) repoCtxt True
              tryUpdateIfFailed globalFlags repoCtxt savedConfig f
          else return result

        goSearchRepos globalFlags repoCtxt savedConfig = do
          tryUpdateIfFailed globalFlags repoCtxt savedConfig searchRepos

        withMaybeRepoCtxt globalFlags mRepoCtxt f
          | Just repoCtxt <- mRepoCtxt
          = f repoCtxt
          | otherwise = withRepoContext verbosity globalFlags f

        withVersions globalFlags mRepoCtxt savedConfig f =
          withMaybeRepoCtxt globalFlags mRepoCtxt $ \repoCtxt ->
            first (gitIndexedRepos repoCtxt) $ \repo -> do
              uris <- remoteBinaryUris (Right repo)
              first uris $ \uri -> do
                case getURIDomain uri of
                  Just domain -> do
                    let indexFile = topLevelIndexFile domain
                    exists <- doesFileExist indexFile
                    ifTrue exists $ do
                      versions <- readLines indexFile
                      f globalFlags repoCtxt savedConfig uri domain versions
                  Nothing -> dieWithError verbosity "Bad uri in index file."

        searchRepos globalFlags repoCtxt savedConfig =
          withVersions globalFlags (Just repoCtxt) savedConfig $
            \globalFlags repoCtxt savedConfig uri domain versions -> do
              first (reverse versions) $ \version -> do
                installVersion globalFlags repoCtxt savedConfig uri domain version True

        installVersion globalFlags repoCtxt savedConfig uri domain version global = do
          let installedFile = etaInstalledFile domain version
              binaryIndexFile = etaBinariesIndexFile domain (Left version)
          exists <- doesFileExist installedFile
          if exists
          then do
            programPaths <- fmap lines $ readFile installedFile
            when global $ selectedVersionMessage verbosity version
            nthProgram n programPaths
          else do
            exists <- doesFileExist binaryIndexFile
            ifTrue exists $ do
              programs <- readLines binaryIndexFile
              programPaths <- downloadPrograms repoCtxt domain uri version programs
              selectedVersionMessage verbosity version
              installBootLibraries verbosity (readVersion version)
                repoCtxt savedConfig globalFlags programPaths
              let pathsFile = unlines programPaths
              writeFile installedFile $ pathsFile
              when global $ writeFile etaPointerFile $ pathsFile
              nthProgram n programPaths

        selectVersion globalFlags repoCtxt savedConfig version =
          tryUpdateIfFailed globalFlags repoCtxt savedConfig $
            \globalFlags repoCtxt savedConfig ->
            withVersions globalFlags (Just repoCtxt) savedConfig $
              \globalFlags repoCtxt savedConfig uri domain versions -> do
              if version `elem` versions
              then installVersion globalFlags repoCtxt savedConfig uri domain version False
              else return Nothing

        selectedVersionMessage verbosity version =
          notice verbosity $
            "Selected " ++ userReadableVersion version ++ "."

        nthProgram :: (Monad m) => Int -> [FilePath] -> m (Maybe (FilePath, [FilePath]))
        nthProgram n programPaths =
          case nth n programPaths of
            -- TODO: Improve this for change monitoring
            Just path -> do
              return $ Just (path, [])
            _         -> return Nothing

        downloadPrograms repoCtxt domain uri version programs = do
          transport <- repoContextGetTransport repoCtxt
          forM programs $ \prog -> do
            notice verbosity $ "Downloading executable '" ++ prog ++ "'..."
            let progFile = etaProgFile domain prog (Left version)
            _ <- downloadURIWithMsg ("Failed to download executable '" ++ prog ++ "'.")
                   transport verbosity
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
  where handler _ = info verbosity $ msg ++ " - " ++ uriToString id uri ""

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
      -- TODO: This seems to override the saved config flags. Fix this.
      configFlags = savedConfigureFlags config `mappend`
                    configFlags' { configDistPref = toFlag dist
                                 , configHcPath   = maybeToFlag $ nth 0 programPaths
                                 , configHcPkg    = maybeToFlag $ nth 1 programPaths }

      configExFlags  = defaultConfigExFlags         `mappend`
                         savedConfigureExFlags config `mappend` configExFlags'
      haddockFlags   = defaultHaddockFlags          `mappend`
                         savedHaddockFlags     config `mappend`
                         haddockFlags' { haddockDistPref = toFlag dist }
      packageDBs = configPackageDB' configFlags

  (comp, platform, progdb') <- configCompilerAux' configFlags
  progdb <- configureAllKnownPrograms verbosity progdb'
  installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
  let missing = numMissingBootLibraries installedPkgIndex
  if missing > 0
  then do
    -- TODO: Optimize this to only attempt to install the missing packages.
    --       Without reinstalls.
    notice verbosity "Installing boot libraries..."
    let installFlags = Setup.defaultInstallFlags      `mappend`
                        savedInstallFlags     config `mappend` installFlags' {
                          installAllowBootLibInstalls =
                            toFlag (AllowBootLibInstalls True),
                          installOverrideReinstall =
                            toFlag True
                        }
    forM_ paths $ \path -> do
      install verbosity packageDBs repos comp platform progdb NoSandbox
        Nothing globalFlags configFlags configExFlags installFlags
        haddockFlags [UserTargetLocalTarball path True]
    notice verbosity "Finished installing boot libraries."
  else notice verbosity "Boot libraries already installed."

numMissingBootLibraries :: InstalledPackageIndex -> Int
numMissingBootLibraries pkgIdx = length
                               $ filter
                                 (\pkg ->
                                     case PackageIndex.searchByName pkgIdx pkg of
                                       PackageIndex.None -> True
                                       _                 -> False)
                               $ bootPackages
bootPackages :: [String]
bootPackages = ["rts", "base", "ghc-prim", "integer", "template-haskell"]

etlasVersion :: String
etlasVersion =  "etlas-" ++ display (mkVersion' Paths_etlas.version)

findTool :: FilePath -> GlobalFlags -> Verbosity -> IO ()
findTool relPath globalFlags' verbosity = do
  savedConfig <- fmap snd $ loadConfigOrSandboxConfig verbosity globalFlags'
  let globalFlags = savedGlobalFlags savedConfig `mappend` globalFlags'
  withRepoContext verbosity globalFlags $ \repoCtxt -> do
    transport <- repoContextGetTransport repoCtxt
    let gitRepos = gitIndexedRepos repoCtxt
    result <- untilM (\repo -> do
                         uris <- remoteBinaryUris (Right repo)
                         untilM (downloadTool verbosity transport relPath) uris)
              gitRepos
    case result of
      Just _  -> return ()
      Nothing -> die' verbosity $ "Failed to download " ++ relPath ++ "."

findCoursier, findVerify :: GlobalFlags -> Verbosity -> IO ()
findCoursier = findTool "coursier"
findVerify   = findTool "classes/Verify.class"

untilM :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe b)
untilM f elems = case elems of
  x:xs -> do
    result <- f x
    case result of
      mb@(Just _) -> return mb
      Nothing     -> untilM f xs
  _ -> return Nothing

downloadTool :: Verbosity -> HttpTransport -> FilePath -> URI -> IO (Maybe ())
downloadTool verbosity transport relPath uri = do
  etlasToolsDir <- defaultEtlasToolsDir
  let etlasToolsDest = etlasToolsDir </> relPath
  createDirectoryIfMissingVerbose verbosity True (takeDirectory etlasToolsDest)
  mResult <- downloadURIAllowFail (\(e :: SomeException) -> print e) transport verbosity
               (uriWithPath uri (etlasVersion </> "tools" </> relPath))
               etlasToolsDest
  return $
    if isNothing mResult
    then Just ()
    else Nothing
