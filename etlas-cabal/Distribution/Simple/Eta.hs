{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Distribution.Simple.Eta (
        configure, getInstalledPackages, getPackageDBContents,
        buildLib, buildExe,
        replLib, replExe,
        startInterpreter,
        installLib, installExe,
        libAbiHash,
        hcPkgInfo,
        registerPackage,
        componentGhcOptions,
        getLibDir,
        isDynamic,
        getGlobalPackageDB,
        -- runCmd
        mkMergedClassPath,
        classPathSeparator,
        mkJarName
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Simple.GHC.ImplInfo
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.PackageDescription as PD
import Distribution.InstalledPackageInfo hiding (frameworks, exposedModules)
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.Simple.PackageIndex ( InstalledPackageIndex )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Simple.Program
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup hiding ( Flag )
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Compiler hiding ( Flag )
import Distribution.Version
import Distribution.System
import Distribution.Verbosity
import Distribution.Utils.NubList
import Distribution.Text
import Distribution.Types.UnitId

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad
import System.Directory hiding (findFile)
import System.FilePath

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramDb
          -> IO (Compiler, Maybe Platform, ProgramDb)
configure verbosity hcPath hcPkgPath conf0 = do
  (etaProg, etaVersion, conf1) <-
    requireProgramVersion verbosity etaProgram
      anyVersion --(orLaterVersion (Version [0,1] []))
      (userMaybeSpecifyPath "eta" hcPath conf0)

  let implInfo = etaVersionImplInfo etaVersion etaGhcVersion

  -- This is slightly tricky, we have to configure eta first, then we use the
  -- location of eta to help find eta-pkg in the case that the user did not
  -- specify the location of eta-pkg directly:
  (etaPkgProg, etaPkgVersion, conf2) <-
    requireProgramVersion verbosity etaPkgProgram
    {- TODO: Is this necessary? {programFindLocation = guessEtaPkgFromEtaPath etaProg} -}
    anyVersion (userMaybeSpecifyPath "eta-pkg" hcPkgPath conf1)

  -- Just etaPkgEtaVersion <- findEtaPkgEtaVersion
  --                                 verbosity (programPath etaPkgProg)

  when (etaVersion /= etaPkgVersion) $ die' verbosity $
       "Version mismatch between eta and eta-pkg: "
    ++ programPath etaProg ++ " is version " ++ display etaVersion ++ " "
    ++ programPath etaPkgProg ++ " is version " ++ display etaPkgVersion

  -- when (etaGhcVersion /= etaPkgVersion) $ die $
  --      "Version mismatch between eta and eta-pkg: "
  --   ++ programPath etaProg
  --   ++ " was built with GHC version " ++ display etaGhcVersion ++ " "
  --   ++ programPath etaPkgProg
  --   ++ " was built with GHC version " ++ display etaPkgVersion

  -- be sure to use our versions of hsc2hs, c2hs, haddock and ghc
  -- let hsc2hsProgram' =
  --       hsc2hsProgram { programFindLocation =
  --                         guessHsc2hsFromEtaPath etaProg }
  --     c2hsProgram' =
  --       c2hsProgram { programFindLocation =
  --                         guessC2hsFromEtaPath etaProg }

  --     haddockProgram' =
  --       haddockProgram { programFindLocation =
  --                         guessHaddockFromEtaPath etaProg }
  --     conf3 = addKnownPrograms [ hsc2hsProgram', c2hsProgram', haddockProgram' ] conf2
  let conf3 = conf2 -- TODO: Account for other programs

  languages  <- Internal.getLanguages  verbosity implInfo etaProg
  extensions <- Internal.getExtensions verbosity implInfo etaProg

  etaInfo <- Internal.getGhcInfo verbosity implInfo etaProg

  let comp = Compiler {
        compilerId         = CompilerId Eta etaVersion,
        -- TODO: Make this unique for ETA?
        compilerAbiTag     = AbiTag $
          "ghc" ++ intercalate "_" (map show . versionNumbers $ etaGhcVersion),
        compilerCompat     = [CompilerId GHC etaGhcVersion],
        compilerLanguages  = languages,
        compilerExtensions = extensions,
        compilerProperties = Map.fromList etaInfo
      }
      compPlatform = Nothing -- Internal.targetPlatform ghcInfo
  (_, conf4) <- requireProgram verbosity javaProgram conf3
  (_, conf5) <- requireProgram verbosity javacProgram conf4
  return (comp, compPlatform, conf5)

-- | Given a single package DB, return all installed packages.
getPackageDBContents :: Verbosity -> PackageDB -> ProgramDb
                     -> IO InstalledPackageIndex
getPackageDBContents verbosity packagedb progdb = do
  pkgss <- getInstalledPackages' verbosity [packagedb] progdb
  toPackageIndex verbosity pkgss progdb

-- | Given a package DB stack, return all installed packages.
getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramDb
                     -> IO InstalledPackageIndex
getInstalledPackages verbosity packagedbs progdb = do
  checkPackageDbEnvVar verbosity
  checkPackageDbStack verbosity packagedbs
  pkgss <- getInstalledPackages' verbosity packagedbs progdb
  index <- toPackageIndex verbosity pkgss progdb
  return $! index

toPackageIndex :: Verbosity
               -> [(PackageDB, [InstalledPackageInfo])]
               -> ProgramDb
               -> IO InstalledPackageIndex
toPackageIndex _ pkgss _ = do
  let indices = [ PackageIndex.fromList pkgs | (_, pkgs) <- pkgss ]
  return $! (mconcat indices)

checkPackageDbEnvVar :: Verbosity -> IO ()
checkPackageDbEnvVar verbosity =
    Internal.checkPackageDbEnvVar verbosity "ETA" "ETA_PACKAGE_PATH"

checkPackageDbStack :: Verbosity -> PackageDBStack -> IO ()
checkPackageDbStack _ (GlobalPackageDB:rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStack verbosity rest
  | GlobalPackageDB `notElem` rest =
  die' verbosity $ "With current ghc versions the global package db is always used "
                ++ "and must be listed first. This ghc limitation may be lifted in "
                ++ "future, see http://hackage.haskell.org/trac/ghc/ticket/5977"
checkPackageDbStack verbosity _ =
  die' verbosity $ "If the global package db is specified, it must be "
                ++ "specified first and cannot be specified multiple times"

getInstalledPackages' :: Verbosity -> [PackageDB] -> ProgramDb
                      -> IO [(PackageDB, [InstalledPackageInfo])]
getInstalledPackages' verbosity packagedbs progdb =
  sequence
    [ do pkgs <- HcPkg.dump (hcPkgInfo progdb) verbosity packagedb
         return (packagedb, pkgs)
    | packagedb <- packagedbs ]

getLibDir :: Verbosity -> LocalBuildInfo -> IO FilePath
getLibDir verbosity lbi =
    (reverse . dropWhile isSpace . reverse) `fmap`
     getDbProgramOutput verbosity etaProgram
     (withPrograms lbi) ["--print-libdir"]

-- getLibDir' :: Verbosity -> ConfiguredProgram -> IO FilePath
-- getLibDir' verbosity etaProg =
--     (reverse . dropWhile isSpace . reverse) `fmap`
--      getProgramOutput verbosity etaProg ["--print-libdir"]

-- | Return the 'FilePath' to the global GHC package database.
getGlobalPackageDB :: Verbosity -> ConfiguredProgram -> IO FilePath
getGlobalPackageDB verbosity etaProg =
    (reverse . dropWhile isSpace . reverse) `fmap`
     getProgramOutput verbosity etaProg ["--print-global-package-db"]

buildLib, replLib :: Verbosity -> Cabal.Flag (Maybe Int) -> PackageDescription
                  -> LocalBuildInfo -> Library -> ComponentLocalBuildInfo
                  -> IO ()
buildLib = buildOrReplLib False
replLib  = buildOrReplLib True

buildOrReplLib :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Library            -> ComponentLocalBuildInfo -> IO ()
buildOrReplLib forRepl verbosity numJobs pkgDescr lbi lib clbi = do
  let uid = componentUnitId clbi
      libTargetDir = buildDir lbi
      isVanillaLib = not forRepl && withVanillaLib lbi
      isSharedLib = not forRepl && withSharedLib lbi
      comp = compiler lbi

  (etaProg, _) <- requireProgram verbosity etaProgram (withPrograms lbi)
  etlasDir <- defaultCabalDir
  (javaProg, _) <- requireProgram verbosity javaProgram (withPrograms lbi)
  let runEtaProg          = runGHC verbosity etaProg comp (hostPlatform lbi)
      libBi               = libBuildInfo lib
      coursierPath        = etlasDir </> "coursier"
      runCoursier options = getProgramInvocationOutput verbosity
                              (programInvocation javaProg $
                                (["-jar", "-noverify", coursierPath] ++ options))

  (depJars, mavenDeps') <- getDependencyClassPaths (installedPkgs lbi)
                            pkgDescr lbi clbi

  let mavenDeps = mavenDeps' ++ extraLibs libBi
      mavenRepos = frameworks libBi
      mavenResolvedRepos = concatMap (\r -> ["-r", r]) $ map resolveOrId mavenRepos

  maybeMavenOutput <- if null mavenDeps
                      then return Nothing
                      else fmap Just (runCoursier $ "fetch"
                                                  : (mavenResolvedRepos ++
                                                     mavenDeps))

  let mavenPaths = case maybeMavenOutput of
        Just mavenOutput -> dropWhile ((/= '/') . head) $ lines mavenOutput
        Nothing          -> []

      fullClassPath = depJars ++ mavenPaths

  createDirectoryIfMissingVerbose verbosity True libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?
  let javaSrcs    = javaSources libBi
      baseOpts    = componentGhcOptions verbosity lbi libBi clbi libTargetDir
      linkJavaLibOpts = mempty {
                          ghcOptInputFiles = toNubListR javaSrcs,
                          ghcOptExtra      = toNubListR $
                            ["-cp", mkMergedClassPath lbi fullClassPath]
                      }
      vanillaOptsNoJavaLib = baseOpts `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptNumJobs      = numJobs,
                      ghcOptInputModules = toNubListR $ allLibModules lib clbi,
                      ghcOptOutputFile   = toFlag target
                    }
      vanillaOpts' = vanillaOptsNoJavaLib `mappend` linkJavaLibOpts
      sharedOpts  = vanillaOpts' `mappend` mempty {
                        ghcOptShared = toFlag True,
                        ghcOptExtra       = toNubListR $
                                            etaSharedOptions libBi
                      }
      vanillaOpts = vanillaOpts' {
                        ghcOptExtraDefault = toNubListR ["-staticlib"]
                    }
      target = libTargetDir </> mkJarName uid

      runVerify = runProgramInvocation verbosity
                    (programInvocation javaProg $
                     ["-cp", mkMergedClassPath lbi (etlasDir : fullClassPath)
                     , "Verify", target])

  unless (forRepl || (null (allLibModules lib clbi) && null javaSrcs)) $ do
       let withVerify act = do
             _ <- act
             when (fromFlagOrDefault False (configVerifyMode $ configFlags lbi)) $
               runVerify
       if isVanillaLib
       then withVerify $ runEtaProg vanillaOpts
       else if isSharedLib
       then withVerify $ runEtaProg sharedOpts
       else return ()

-- | Start a REPL without loading any source files.
startInterpreter :: Verbosity -> ProgramDb -> Compiler -> Platform
                 -> PackageDBStack -> IO ()
startInterpreter verbosity progdb comp platform packageDBs = do
  let replOpts = mempty {
        ghcOptMode       = toFlag GhcModeInteractive,
        ghcOptPackageDBs = packageDBs
        }
  checkPackageDbStack verbosity packageDBs
  (etaProg, _) <- requireProgram verbosity etaProgram progdb
  runGHC verbosity etaProg comp platform replOpts

buildExe, replExe :: Verbosity          -> Cabal.Flag (Maybe Int)
                  -> PackageDescription -> LocalBuildInfo
                  -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe = buildOrReplExe False
replExe  = buildOrReplExe True

buildOrReplExe :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildOrReplExe _forRepl verbosity numJobs pkgDescr lbi
  exe@Executable { exeName, modulePath = modPath } clbi = do
  let exeName'    = display exeName
      exeNameReal = exeName' <.> (if takeExtension exeName' /= ('.':jarExtension)
                                  then jarExtension
                                  else "")
      targetDir   = buildDir lbi </> exeName'
      exeDir      = targetDir </> (exeName' ++ "-tmp")
      exeJar      = targetDir </> exeNameReal

  (etaProg, _)  <- requireProgram verbosity etaProgram  (withPrograms lbi)
  (javaProg, _) <- requireProgram verbosity javaProgram (withPrograms lbi)
  etlasDir <- defaultCabalDir
  let runEtaProg          = runGHC verbosity etaProg comp (hostPlatform lbi)
      coursierPath        = etlasDir </> "coursier"
      runCoursier options = getProgramInvocationOutput verbosity
                              (programInvocation javaProg $
                                (["-jar", "-noverify", coursierPath] ++ options))

  createDirectoryIfMissingVerbose verbosity True exeDir

  srcMainFile <- findFile (hsSourceDirs exeBi) modPath

  (depJars, mavenDeps') <- getDependencyClassPaths (installedPkgs lbi)
                            pkgDescr lbi clbi

  let mavenDeps = mavenDeps' ++ (extraLibs . buildInfo $ exe)
      mavenRepos = frameworks . buildInfo $ exe
      mavenResolvedRepos = concatMap (\r -> ["-r", r]) $ map resolveOrId mavenRepos

  maybeMavenOutput <- if null mavenDeps
                      then return Nothing
                      else fmap Just (runCoursier $ "fetch" : "--quiet"
                                                  : (mavenResolvedRepos ++
                                                     mavenDeps))

  let mavenPaths = case maybeMavenOutput of
        Just mavenOutput -> lines mavenOutput
        Nothing          -> []
      javaSrcs = (if isShared
                  then []
                  else mavenPaths) ++ javaSrcs'

      fullClassPath = depJars ++ mavenPaths

      classPaths' = if isShared then fullClassPath else []
      dirEnvVar = "DIR"
      dirEnvVarRef = if isWindows' then "%" ++ dirEnvVar ++ "%" else "$" ++ dirEnvVar
      classPaths = (if isShared && not (null javaSrcs)
                    then [dirEnvVarRef ++ "/" ++ exeName' ++ "-tmp/__extras.jar"]
                    else [])
                    ++ classPaths'
      baseOpts = (componentGhcOptions verbosity lbi exeBi clbi exeDir)
                 `mappend` mempty {
                   ghcOptMode         = toFlag GhcModeMake,
                   ghcOptInputFiles   = toNubListR $ srcMainFile : javaSrcs,
                   ghcOptInputModules = toNubListR $ exeModules exe,
                   ghcOptNumJobs      = numJobs,
                   ghcOptOutputFile   = toFlag exeJar,
                   ghcOptShared       = toFlag isShared,
                   ghcOptExtra        = toNubListR $
                     ["-cp", mkMergedClassPath lbi fullClassPath]
                 }

      verifyClassPath = mkMergedClassPath lbi (etlasDir : exeJar : classPaths)

      runVerify = runProgramInvocation verbosity
                    (programInvocation javaProg $
                     ["-cp", verifyClassPath, "Verify", exeJar])

      withVerify act = do
        _ <- act
        when (fromFlagOrDefault False (configVerifyMode $ configFlags lbi)) $
          runVerify

  withVerify $ runEtaProg baseOpts

  -- Generate command line executable file
  let classPaths''= if null classPaths then ""
                     else classPathSep : mkMergedClassPath lbi classPaths
      generateExeScript =
        if isWindows' then
             "@echo off\r\n"
          ++ "set " ++ dirEnvVar ++ "=%~dp0\r\n"
          ++ "java -classpath \"" ++ dirEnvVarRef ++ "/" ++ exeNameReal
          ++ classPaths''
          ++ "\" eta.main %*\r\n"
        else
             "#!/usr/bin/env bash\n"
          ++ dirEnvVar ++ "=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"\n"
          ++ "java -classpath \"" ++ dirEnvVarRef ++ "/" ++ exeNameReal
          ++ classPaths''
          ++ "\" eta.main \"$@\"\n"
      scriptFile = targetDir </> exeName'
                               ++ if isWindows' then ".cmd" else ""
  writeUTF8File scriptFile generateExeScript
  p <- getPermissions scriptFile
  setPermissions scriptFile (p {executable = True})
  where comp         = compiler lbi
        exeBi        = buildInfo exe
        isShared     = withDynExe lbi
        javaSrcs'    = javaSources exeBi
        isWindows'   = isWindows lbi
        classPathSep = head (classPathSeparator lbi)

isWindows :: LocalBuildInfo -> Bool
isWindows lbi | Platform _ Windows <- hostPlatform lbi = True
              | otherwise = False

-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib    :: Verbosity
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic libraries
              -> FilePath  -- ^Build location
              -> PackageDescription
              -> Library
              -> ComponentLocalBuildInfo
              -> IO ()
installLib verbosity lbi targetDir _dynlibTargetDir builtDir _pkg lib clbi = do
  copyModuleFiles "hi"
  when hasLib $ mapM_ (installOrdinary builtDir targetDir) jarLibNames
  where
    install _isShared srcDir dstDir name = do
      createDirectoryIfMissingVerbose verbosity True dstDir
      installOrdinaryFile   verbosity src dst
      where src = srcDir </> name
            dst = dstDir </> name

    installOrdinary = install False
    _installShared   = install True

    copyModuleFiles ext =
      findModuleFiles [builtDir] [ext] (allLibModules lib clbi)
      >>= installOrdinaryFiles verbosity targetDir

    _cid = compilerId (compiler lbi)
    libUids = [componentUnitId clbi]
    jarLibNames = map mkJarName libUids

    hasLib    = not $ null (allLibModules lib clbi)
                   && null (javaSources (libBuildInfo lib))

mkJarName :: UnitId -> String
mkJarName uid = getHSLibraryName uid <.> "jar"

installExe :: Verbosity
              -> LocalBuildInfo
              -> InstallDirs FilePath -- ^Where to copy the files to
              -> FilePath  -- ^Build location
              -> (FilePath, FilePath)  -- ^Executable (prefix,suffix)
              -> PackageDescription
              -> Executable
              -> IO ()
installExe verbosity lbi installDirs buildPref
           (_progprefix, _progsuffix) _pkg exe = do
  let binDir = bindir installDirs
      toDir x = binDir </> x
      exeName' = display (exeName exe)
      buildDir = buildPref </> exeName'
      fromDir x = buildDir </> x
      exeNameExt ext = if null ext
                       then exeName'
                       else exeName' <.> ext
      launchExt = if isWindows lbi then "cmd" else ""
      copy x = copyFile (fromDir x) (toDir x)
  createDirectoryIfMissingVerbose verbosity True binDir
  copy (exeNameExt launchExt)
  copy (exeNameExt "jar")
  --copyFile (fromDir (exeNameExt "jar")) (toDir (progprefix ++ exeName exe ++ progsuffix))

libAbiHash :: Verbosity -> PackageDescription -> LocalBuildInfo
           -> Library -> ComponentLocalBuildInfo -> IO String
libAbiHash verbosity _pkg_descr lbi lib clbi = do
  let
      libBi       = libBuildInfo lib
      comp        = compiler lbi
      platform    = hostPlatform lbi
      vanillaArgs =
        (componentGhcOptions verbosity lbi libBi clbi (buildDir lbi))
        `mappend` mempty {
          ghcOptMode         = toFlag GhcModeAbiHash,
          ghcOptInputModules = toNubListR $ exposedModules lib
        }
      ghcArgs = if withVanillaLib lbi then vanillaArgs
                else error "libAbiHash: Can't find an enabled library way"
  (etaProg, _) <- requireProgram verbosity etaProgram (withPrograms lbi)
  getProgramInvocationOutput verbosity (ghcInvocation etaProg comp platform ghcArgs)

registerPackage :: Verbosity
                -> ProgramDb
                -> HcPkg.MultiInstance
                -> PackageDBStack
                -> InstalledPackageInfo
                -> IO ()
registerPackage verbosity progdb multiInstance packageDbs installedPkgInfo
  | HcPkg.MultiInstance <- multiInstance
  = HcPkg.registerMultiInstance (hcPkgInfo progdb) verbosity
      packageDbs installedPkgInfo
  | otherwise
  = HcPkg.reregister (hcPkgInfo progdb) verbosity
      packageDbs (Right installedPkgInfo)

componentGhcOptions :: Verbosity -> LocalBuildInfo
                    -> BuildInfo -> ComponentLocalBuildInfo -> FilePath
                    -> GhcOptions
componentGhcOptions verbosity lbi bi clbi odir =
  let opts = Internal.componentGhcOptions verbosity implInfo lbi bi clbi odir
      comp = compiler lbi
      implInfo = getImplInfo comp
  in opts
    {
      ghcOptExtra = ghcOptExtra opts
        `mappend` toNubListR (["-pgmjavac", javacPath] ++ (hcOptions Eta bi))
    }
  where Just javacProg = lookupProgram javacProgram (withPrograms lbi)
        javacPath = locationPath (programLocation javacProg)

-- etaProfOptions :: BuildInfo -> [String]
-- etaProfOptions bi =
--   hcProfOptions GHC bi `mappend` hcProfOptions ETA bi

etaSharedOptions :: BuildInfo -> [String]
etaSharedOptions bi =
  hcSharedOptions GHC bi `mappend` hcSharedOptions Eta bi

-- TODO: Correct default?
isDynamic :: Compiler -> Bool
isDynamic = const True

-- findEtaGhcVersion :: Verbosity -> FilePath -> IO (Maybe Version)
-- findEtaGhcVersion verbosity pgm =
--   findProgramVersion "--numeric-ghc-version" id verbosity pgm

-- findEtaPkgEtaVersion :: Verbosity -> FilePath -> IO (Maybe Version)
-- findEtaPkgEtaVersion verbosity pgm =
--   findProgramVersion "--numeric-eta-version" id verbosity pgm

-- -----------------------------------------------------------------------------
-- Registering

hcPkgInfo :: ProgramDb -> HcPkg.HcPkgInfo
hcPkgInfo progdb = HcPkg.HcPkgInfo { HcPkg.hcPkgProgram    = etaPkgProg
                                   , HcPkg.noPkgDbStack    = False
                                   , HcPkg.noVerboseFlag   = False
                                   , HcPkg.flagPackageConf = False
                                   , HcPkg.supportsDirDbs  = True
                                   , HcPkg.requiresDirDbs  = True
                                   , HcPkg.nativeMultiInstance  = True
                                   , HcPkg.recacheMultiInstance = True
                                   }
  where Just etaPkgProg = lookupProgram etaPkgProgram progdb

-- NOTE: ETA is frozen after 7.10.3
etaGhcVersion :: Version
etaGhcVersion = mkVersion [7,10,3]

jarExtension :: String
jarExtension = "jar"

getDependencyClassPaths
  :: InstalledPackageIndex
  -> PackageDescription
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> IO ([FilePath], [String])
getDependencyClassPaths packageIndex pkgDescr lbi clbi = do
  libs' <- fmap concat $ mapM hsLibraryPaths packageInfos
  return (libPaths ++ libs', libMavenDeps ++ mavenDeps)
  where mavenDeps = concatMap InstalledPackageInfo.extraLibraries packageInfos
        packages' = map fst $ componentPackageDeps clbi
        (libs, packages'') = partition (isInfixOf "-inplace" . show) packages'
        dirEnvVarRef = if isWindows lbi then "%DIR%" else "$DIR"
        libPaths = if null libs then [] else [dirEnvVarRef ++ "/../"
                                              ++ mkJarName (fromJust mbLibUid)]
        libMavenDeps = if null libs
                       then []
                       else extraLibs . libBuildInfo . fromJust $ library pkgDescr
        (mbLibUid, libDeps) =
          if null libs
          then (Nothing, [])
          else (\(clbi':_) -> ( Just (componentUnitId clbi')
                              , map fst $ componentPackageDeps clbi' ))
             . fromJust
             -- TODO: When there are multiple component dependencies (Backpack-support)
             --       this might break. -RM
             . Map.lookup CLibName
             $ componentNameMap lbi

        packages = libDeps ++ packages''

        closurePackageIndex = case PackageIndex.dependencyClosure packageIndex packages of
          Left pkgIdx -> pkgIdx
          Right errs -> error $ show ("deps error", errs)

        packageInfos = PackageIndex.allPackages closurePackageIndex
        hsLibraryPaths pinfo = mapM (findFile (libraryDirs pinfo))
                                    (map (<.> "jar") $ hsLibraries pinfo)

builtInMavenResolvers :: [(String, String)]
builtInMavenResolvers =
  [ ("central", "https://repo1.maven.org/maven2/")
  , ("javaNet1", "http://download.java.net/maven/1/")
  , ("sonatype:public", "https://oss.sonatype.org/content/repositories/public")
  , ("sonatype:snapshots", "https://oss.sonatype.org/content/repositories/snapshots")
  , ("sonatype:releases", "https://oss.sonatype.org/content/repositories/releases")
  , ("jcenter", "https://jcenter.bintray.com/") ]

resolveOrId :: String -> String
resolveOrId repo
  | Just resolved <- lookup repo builtInMavenResolvers
  = resolved
  | "bintray" `isPrefixOf` repo
  = let (_, rest) = break (== ':') repo
        (owner, repos') = break (== ':') $ drop 1 rest
        repos = drop 1 repos'
    in "https://dl.bintray.com/" ++ owner ++ "/" ++ repos ++ "/"
  | otherwise = repo


classPathSeparator :: LocalBuildInfo -> String
classPathSeparator lbi | Platform _ Windows <- hostPlatform lbi = ";"
                       | otherwise = ":"

mkMergedClassPath :: LocalBuildInfo -> [FilePath] -> FilePath
mkMergedClassPath lbi = intercalate (classPathSeparator lbi)

-- TODO: Duplicate definition warning -RM
defaultCabalDir :: IO FilePath
defaultCabalDir = getAppUserDataDirectory "etlas"
