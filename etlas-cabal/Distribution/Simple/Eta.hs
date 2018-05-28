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
        mkJarName,
        JavaExec(..),
        runJava,
        fetchMavenDependencies,
        findCoursierRef,
        findVerifyRef,
        getLibraryComponent,
        getDependencyClassPaths,
        InstallDirType(..),
        exeJarPath,
        libJarPath,
        getInstalledPackagesMonitorFiles
  ) where

import Prelude ()
import Data.IORef
import System.IO.Unsafe
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
import Distribution.Simple.InstallDirs (defaultEtlasDir, defaultEtlasToolsDir)
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
import Distribution.Types.UnqualComponentName
import qualified Paths_etlas_cabal as Etlas (version)

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

trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

getLibDir :: Verbosity -> ProgramDb -> IO FilePath
getLibDir verbosity progDb =
  trimEnd `fmap` getDbProgramOutput verbosity etaProgram progDb ["--print-libdir"]

-- getLibDir' :: Verbosity -> ConfiguredProgram -> IO FilePath
-- getLibDir' verbosity etaProg =
--     (reverse . dropWhile isSpace . reverse) `fmap`
--      getProgramOutput verbosity etaProg ["--print-libdir"]

-- | Return the 'FilePath' to the global GHC package database.
getGlobalPackageDB :: Verbosity -> ConfiguredProgram -> IO FilePath
getGlobalPackageDB verbosity etaProg
  | Just version <- programVersion etaProg
  , version >= mkVersion [0,7,1]
  = trimEnd `fmap` getProgramOutput verbosity etaProg ["--print-global-package-db"]
  | otherwise
  = ((</> "package.conf.d") . trimEnd)
    `fmap` getProgramOutput verbosity etaProg ["--print-libdir"]

buildLib, replLib :: Verbosity -> Cabal.Flag (Maybe Int) -> PackageDescription
                  -> LocalBuildInfo -> Library -> ComponentLocalBuildInfo
                  -> IO ()
buildLib = buildOrReplLib False
replLib  = buildOrReplLib True

libJarPath :: LocalBuildInfo -> ComponentLocalBuildInfo -> FilePath
libJarPath lbi clbi = buildDir lbi </> mkJarName (componentUnitId clbi)

buildOrReplLib :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Library            -> ComponentLocalBuildInfo -> IO ()
buildOrReplLib forRepl verbosity numJobs pkgDescr lbi lib clbi = do
  let uid = componentUnitId clbi
      libTargetDir = buildDir lbi
      isVanillaLib = not forRepl && withVanillaLib lbi
      isSharedLib = not forRepl && withSharedLib lbi
      ifReplLib = when forRepl
      comp = compiler lbi

  (etaProg, progDb0) <- requireProgram verbosity etaProgram (withPrograms lbi)
  etaServPath        <- findEtaServ verbosity progDb0
  let runEtaProg          = runGHC verbosity etaProg comp (hostPlatform lbi)
      libBi               = libBuildInfo lib

  doWithResolvedDependencyClassPathsOrDie verbosity pkgDescr lbi clbi libBi
    RelativeInstallDir $ \ (depJars,mavenPaths) -> do

      let fullClassPath = depJars ++ mavenPaths

      createDirectoryIfMissingVerbose verbosity True libTargetDir
      -- TODO: do we need to put hs-boot files into place for mutually recursive
      -- modules?

      let javaSrcs    = javaSources libBi
          baseOpts    = componentGhcOptions verbosity lbi libBi clbi libTargetDir
                        `mappend` mempty {
                          ghcOptExtra = toNubListR ["-pgmi", etaServPath]
                        }
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
                            -- Libs should never have the -shared flag
                            -- ghcOptShared = toFlag True,
                            ghcOptExtraDefault = toNubListR ["-staticlib"],
                            ghcOptExtra = toNubListR $ etaSharedOptions libBi
                          }
          vanillaOpts = vanillaOpts' {
                            ghcOptExtraDefault = toNubListR ["-staticlib"]
                        }
          replOpts    = vanillaOpts' {
                          ghcOptExtra        = overNubListR
                                               Internal.filterGhciFlags $
                                               ghcOptExtra vanillaOpts',
                          ghcOptNumJobs      = mempty,
                          ghcOptMode         = toFlag GhcModeInteractive,
                          ghcOptOptimisation = toFlag GhcNoOptimisation
                        }
          target = libTargetDir </> mkJarName uid
      unless (forRepl || (null (allLibModules lib clbi) && null javaSrcs)) $ do
          let withVerify act = do
                _ <- act
                when (fromFlagOrDefault False (configVerifyMode $ configFlags lbi)) $
                  runVerify verbosity fullClassPath target lbi
          if isVanillaLib
          then withVerify $ runEtaProg vanillaOpts
          else if isSharedLib
          then withVerify $ runEtaProg sharedOpts
          else return ()
      ifReplLib $ do
        when (null (allLibModules lib clbi)) $ warn verbosity "No exposed modules"
        ifReplLib (runEtaProg replOpts)

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

exeJarPath :: LocalBuildInfo -> UnqualComponentName -> FilePath
exeJarPath lbi ucn = buildDir lbi </> exeName' </> realExeName exeName'
  where exeName' = display ucn

realExeName :: String -> FilePath
realExeName exeName'
  | takeExtension exeName' /= ('.':jarExtension)
  = exeName' <.> jarExtension
  | otherwise = exeName'

buildOrReplExe :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildOrReplExe forRepl verbosity numJobs pkgDescr lbi
  exe@Executable { exeName, modulePath = modPath } clbi = do
  let exeName'    = display exeName
      exeNameReal = realExeName exeName'
      targetDir   = buildDir lbi </> exeName'
      exeTmpDir   = exeName' ++ "-tmp"
      exeDir      = targetDir </> exeTmpDir
      exeJar      = targetDir </> exeNameReal
      withDeps    = doWithResolvedDependencyClassPathsOrDie
                    verbosity pkgDescr lbi clbi exeBi
  (etaProg, progDb0) <- requireProgram verbosity etaProgram  (withPrograms lbi)
  etaServPath        <- findEtaServ verbosity progDb0
  createDirectoryIfMissingVerbose verbosity True exeDir
  srcMainFile <- findFile (hsSourceDirs exeBi) modPath

  withDeps RelativeInstallDir $ \ (depJars,mavenPaths) -> do

      let fullClassPath = depJars ++ mavenPaths
          classPaths
            | isShared  = fullClassPath
            | otherwise = []
          -- Handle java sources
          javaSrcs
            | isShared  = javaSrcs'
            | otherwise = mavenPaths ++ javaSrcs'

          runEtaProg  = runGHC verbosity etaProg comp (hostPlatform lbi)
          baseOpts = (componentGhcOptions verbosity lbi exeBi clbi exeDir)
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptInputFiles   = toNubListR $ srcMainFile : javaSrcs,
                      ghcOptInputModules = toNubListR $ exeModules exe,
                      ghcOptNumJobs      = numJobs,
                      ghcOptOutputFile   = toFlag exeJar,
                      ghcOptShared       = toFlag isShared,
                      ghcOptExtra        = toNubListR $
                        ["-cp", mkMergedClassPath lbi fullClassPath,
                         "-pgmi", etaServPath]
                    }
          replOpts = baseOpts `mappend` mempty {
                       ghcOptMode         = toFlag GhcModeInteractive,
                       ghcOptOptimisation = toFlag GhcNoOptimisation
                     }
          withVerify act = do
            _ <- act
            when (fromFlagOrDefault False (configVerifyMode $ configFlags lbi)) $
              runVerify verbosity (exeJar : classPaths) exeJar lbi
      if forRepl
      then runEtaProg replOpts
      else do withVerify $ runEtaProg baseOpts
              -- Generate command line file / jar exec launchers
              generateExeLaunchers verbosity lbi exeName' classPaths targetDir
  -- Generate launchers for the install phase
  unless forRepl $ withDeps AbsoluteGlobalInstallDir $
    \ (depJars, mavenPaths) -> do
      let installLaunchersDir = targetDir </> "install-launchers"
          classPaths
            | isShared  = depJars ++ mavenPaths
            | otherwise = []
      createDirectoryIfMissingVerbose verbosity True installLaunchersDir
      generateExeLaunchers verbosity lbi exeName' classPaths installLaunchersDir


  where comp         = compiler lbi
        exeBi        = buildInfo exe
        isShared     = withDynExe lbi
        javaSrcs'    = javaSources exeBi

dirEnvVarAndRef :: Bool -> (String,String)
dirEnvVarAndRef isWindows' = (var,ref)
  where var = "DIR"
        ref | isWindows' = "%" ++ var ++ "%"
            | otherwise  = "$" ++ var

generateExeLaunchers :: Verbosity -> LocalBuildInfo -> String
                     -> [String]  -> FilePath       -> IO ()
generateExeLaunchers verbosity lbi exeName classPaths targetDir = do
  generateExeLauncherScript verbosity lbi exeName classPaths targetDir
  {- Windows faces the dreaded line-length limit which forces us to create a
     launcher jar as a workaround. -}
  when (isWindows lbi) $
    generateExeLauncherJar verbosity lbi exeName classPaths targetDir

generateExeLauncherScript :: Verbosity -> LocalBuildInfo -> String
                          -> [String]  -> FilePath       -> IO ()
generateExeLauncherScript verbosity lbi exeName classPaths targetDir = do
  let isWindows'   = isWindows lbi
      classPathSep = head (classPathSeparator lbi)
      scriptClassPaths
        | null classPaths = ""
        | otherwise = mkMergedClassPath lbi classPaths
      exeScript = exeLauncherScript classPathSep exeName
                  scriptClassPaths isWindows'
      scriptFile | isWindows' = prefix ++ ".cmd"
                 | otherwise  = prefix
        where prefix = targetDir </> exeName

  writeUTF8File scriptFile exeScript
  p <- getPermissions scriptFile
  setPermissions scriptFile (p { executable = True })

exeLauncherScript :: Char -> String -> String -> Bool -> String
exeLauncherScript classPathSep exeName classPaths isWindows'
  | isWindows'
  = "@echo off\r\n"
    ++ "set " ++ dirEnvVar ++ "=%~dp0\r\n"
    ++ "if defined ETA_JAVA_CMD goto execute\r\n"
    ++ "if defined JAVA_HOME goto findJavaFromJavaHome\r\n"
    ++ "set ETA_JAVA_CMD=java.exe\r\n"
    ++ "goto execute\r\n"
    ++ ":findJavaFromJavaHome\r\n"
    ++ "set ETA_JAVA_HOME=%JAVA_HOME:\"=%\r\n"
    ++ "set ETA_JAVA_CMD=%ETA_JAVA_HOME%\\bin\\java.exe\r\n"
    ++ ":execute\r\n"
    ++ "\"%ETA_JAVA_CMD%\" %JAVA_ARGS% %JAVA_OPTS% %ETA_JAVA_ARGS% "
    ++ "-classpath " ++ winClassPath ++ " eta.main %*\r\n"
  | otherwise
  = "#!/usr/bin/env bash\n"
    ++ dirEnvVar ++ "=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"\n"
    ++ "if [ -z \"$ETA_JAVA_CMD\" ]; then\n"
    ++ "    if [ -n \"$JAVA_HOME\" ] ; then\n"
    ++ "        if [ -x \"$JAVA_HOME/jre/sh/java\" ] ; then\n"
    ++ "            ETA_JAVA_CMD=\"$JAVA_HOME/jre/sh/java\"\n"
    ++ "        else\n"
    ++ "            ETA_JAVA_CMD=\"$JAVA_HOME/bin/java\"\n"
    ++ "        fi\n"
    ++ "    else\n"
    ++ "        ETA_JAVA_CMD=\"java\"\n"
    ++ "    fi\n"
    ++ "fi\n"
    ++ "$ETA_JAVA_CMD $JAVA_ARGS $JAVA_OPTS $ETA_JAVA_ARGS "
       ++ "-classpath \"" ++ totalClassPath ++ "\" eta.main \"$@\"\n"
  where (dirEnvVar, dirEnvVarRef) = dirEnvVarAndRef isWindows'
        exeJarEnv   = dirEnvVarRef </> realExeName exeName
        totalClassPath = exeJarEnv ++ [classPathSep] ++
                         classPaths ++ [classPathSep] ++ "$ETA_CLASSPATH"
        -- For Windows
        launcherJarEnv = dirEnvVarRef </> (exeName ++ ".launcher.jar")
        winClassPath = "\"" ++ launcherJarEnv ++ "\"" ++ [classPathSep] ++
                         "%ETA_CLASSPATH%"

generateExeLauncherJar :: Verbosity -> LocalBuildInfo -> String
                       -> [String]  -> FilePath       -> IO ()
generateExeLauncherJar verbosity lbi exeName classPaths targetDir = do
  jarProg  <- fmap fst $ requireProgram verbosity jarProgram (withPrograms lbi)
  writeFile targetManifest $ unlines $
    ["Manifest-Version: 1.0"
    ,"Created-By: etlas-" ++ display Etlas.version
    ,"Main-Class: eta.main"
    ,"Class-Path: " ++ realExeName exeName]
    ++ map ((++) "  ") classPaths'
  -- Create the launcher jar
  runProgramInvocation verbosity
    $ programInvocation jarProg ["cfm", launcherJar, targetManifest]
  where addStartingPathSep path | hasDrive path  = pathSeparator : path
                                | otherwise      = path
        (_, dirEnvVarRef) = dirEnvVarAndRef $ isWindows lbi
        replaceEnvVar = replacePrefix dirEnvVarRef "."
        classPaths' = map (addStartingPathSep . replaceEnvVar) classPaths
        targetManifest = targetDir </> "MANIFEST.MF"
        launcherJar = targetDir </> (exeName ++ ".launcher.jar")

isWindows :: LocalBuildInfo -> Bool
isWindows lbi | Platform _ Windows <- hostPlatform lbi = True
              | otherwise = False

replacePrefix :: Eq a => [a] -> [a] -> [a] -> [a]
replacePrefix old new s
  | isPrefixOf old s = new ++ drop (length old) s
  | otherwise = s

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
  when hasLib $ do
    mapM_ (installOrdinaryIfExists builtDir targetDir) jarLibNames
    mapM_ (installOrdinaryIfExists builtDir targetDir) ffiMapNames
  where
    installOrdinaryIfExists srcDir dstDir name = do
      exists <- doesFileExist $ srcDir </> name
      when exists $ installOrdinary srcDir dstDir name

    installOrdinary srcDir dstDir name = do
      createDirectoryIfMissingVerbose verbosity True dstDir
      installOrdinaryFile verbosity src dst
      where src = srcDir </> name
            dst = dstDir </> (fromMaybe name $ stripPrefix "HS" name)
     -- The stripPrefix "HS" bit is for backwards compat

    copyModuleFiles ext =
      findModuleFiles [builtDir] [ext] (allLibModules lib clbi)
      >>= installOrdinaryFiles verbosity targetDir

    _cid = compilerId (compiler lbi)
    libUids = [componentUnitId clbi]
    jarLibNames = map mkJarName libUids ++ map (("HS" ++) . mkJarName) libUids
    ffiMapNames = map mkFFIMapName libUids

    hasLib    = not $ null (allLibModules lib clbi)
                   && null (javaSources (libBuildInfo lib))

mkJarName :: UnitId -> String
mkJarName uid = getHSLibraryName uid <.> "jar"

mkFFIMapName :: UnitId -> String
mkFFIMapName uid = getHSLibraryName uid <.> "ffimap"

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
  let exeBi = buildInfo exe
      binDir = bindir installDirs
      toDir x = binDir </> x
      exeName' = display (exeName exe)
      buildDir = buildPref </> exeName'
      installLaunchersDir = buildDir </> "install-launchers"
      exeNameExt ext = if null ext then exeName'
                       else exeName' <.> ext
      launchExt = if isWindows lbi then "cmd" else ""
      copy fromDir x = copyFile (fromDir </> x) (toDir x)
  createDirectoryIfMissingVerbose verbosity True binDir
  copy buildDir (exeNameExt "jar")
  copy installLaunchersDir (exeNameExt launchExt)
  when (isWindows lbi) $
    copy installLaunchersDir (exeNameExt "launcher.jar")

libAbiHash :: Verbosity -> PackageDescription -> LocalBuildInfo
           -> Library -> ComponentLocalBuildInfo -> IO String
libAbiHash verbosity _pkg_descr lbi lib clbi = do
  let
      libBi       = libBuildInfo lib
      comp        = compiler lbi
      platform    = hostPlatform lbi
      vanillaArgs =
        (componentGhcOptions verbosity lbi libBi clbi (componentBuildDir lbi clbi))
        `mappend` mempty {
          ghcOptMode         = toFlag GhcModeAbiHash,
          ghcOptInputModules = toNubListR $ exposedModules lib
        }
      etaArgs = vanillaArgs
  (etaProg, _) <- requireProgram verbosity etaProgram (withPrograms lbi)
  getProgramInvocationOutput verbosity (ghcInvocation etaProg comp platform etaArgs)

registerPackage :: Verbosity
                -> ProgramDb
                -> PackageDBStack
                -> InstalledPackageInfo
                -> HcPkg.RegisterOptions
                -> IO ()
registerPackage verbosity progdb packageDbs installedPkgInfo registerOptions =
    HcPkg.register (hcPkgInfo progdb) verbosity packageDbs
                   installedPkgInfo registerOptions

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
                                   , HcPkg.suppressFilesCheck = True
                                   }
  where Just etaPkgProg = lookupProgram etaPkgProgram progdb

-- NOTE: ETA is frozen after 7.10.3
etaGhcVersion :: Version
etaGhcVersion = mkVersion [7,10,3]

jarExtension :: String
jarExtension = "jar"

doWithResolvedDependencyClassPathsOrDie
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> BuildInfo
  -> InstallDirType
  -> (([FilePath], [FilePath]) -> IO ())
  -> IO ()
doWithResolvedDependencyClassPathsOrDie verbosity pkgDescr lbi clbi bi libDirType action = do
  mDeps <- getDependencyClassPaths (installedPkgs lbi) pkgDescr lbi clbi bi libDirType
  case mDeps of
    Just (depJars, mavenDeps) -> do
      let mavenRepos = frameworks bi
      mavenPaths <- fetchMavenDependencies verbosity mavenRepos mavenDeps
                      (withPrograms lbi)
      action (depJars,mavenPaths)
    Nothing -> die' verbosity "Missing dependencies. Try `etlas install --dependencies-only`?"

data InstallDirType = RelativeInstallDir
                    | AbsoluteLocalInstallDir
                    | AbsoluteGlobalInstallDir

getDependencyClassPaths
  :: InstalledPackageIndex
  -> PackageDescription
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> BuildInfo
  -> InstallDirType
  -> IO (Maybe ([FilePath], [String]))
getDependencyClassPaths packageIndex pkgDescr lbi clbi bi libDirType
  | Left closurePackageIndex <- closurePackageIndex'
  = do let packageInfos = PackageIndex.allPackages closurePackageIndex
           packageMavenDeps = concatMap InstalledPackageInfo.extraLibraries packageInfos

       packagesPaths <- fmap concat $ mapM hsLibraryPaths packageInfos
       return $ Just (libPath ++ packagesPaths, mavenDeps ++ libMavenDeps ++ packageMavenDeps)

  | otherwise = return Nothing
  where hsLibraryPaths pinfo = mapM (findFile (libraryDirs pinfo))
                                       (map (<.> "jar") $ hsLibraries pinfo)
        closurePackageIndex' = PackageIndex.dependencyClosure packageIndex packages
          where packages  = libDeps ++ packages''
        mavenDeps = extraLibs bi
        libPath
          | null libs = []
          | otherwise = case libDirType of
               RelativeInstallDir        -> [dirEnvVarRef ++ "/../" ++ libJarName]
               AbsoluteLocalInstallDir   -> [buildDir lbi </> libJarName]
               AbsoluteGlobalInstallDir  -> [globalLibDir </> libJarName]
          where (_,dirEnvVarRef) = dirEnvVarAndRef $ isWindows lbi
                libUid = fromJust mbLibUid
                libJarName = mkJarName libUid
                globalLibDir = libdir $ absoluteComponentInstallDirs pkgDescr lbi libUid NoCopyDest
        libMavenDeps
          | null libs = []
          | otherwise = extraLibs . libBuildInfo . fromJust $ library pkgDescr
        (mbLibUid, libDeps)
          | null libs = (Nothing, [])
          | otherwise = (Just $ componentUnitId clbi'
                        , map fst $ componentPackageDeps clbi')
          where clbi' = fromJust libComponent
        (libs, packages'') = maybe ([], packages') (\lc -> partition (== lc) packages') $
                             fmap componentUnitId libComponent
          where packages' = map fst $ componentPackageDeps clbi
        libComponent = getLibraryComponent lbi


getLibraryComponent :: LocalBuildInfo -> Maybe ComponentLocalBuildInfo
getLibraryComponent lbi = fmap head clbis
  where clbis = Map.lookup CLibName $ componentNameMap lbi
              -- TODO: When there are multiple component dependencies
              --       (Backpack-support) this might break. -RM

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

data JavaExec = Jar FilePath | JavaClass String

runJava :: Verbosity -> [String] -> JavaExec -> [String] ->
           ProgramDb -> IO String
runJava verb javaArgs javaExec javaExecArgs progDb = do
  (javaProg,_) <- requireProgram verb javaProgram progDb
  let (exJavaArgs,javaExec')  = case javaExec of
                                 Jar path -> (["-jar"],path)
                                 JavaClass name -> ([],name)
      javaInv = programInvocation javaProg $ javaArgs ++ exJavaArgs
      javaInv' = withSystemProxySetting javaInv
      javaExecInv = simpleProgramInvocation javaExec' javaExecArgs
  getProgramInvocationOutput verb $ nestedProgramInvocation javaInv' javaExecInv

withSystemProxySetting :: ProgramInvocation -> ProgramInvocation
withSystemProxySetting  javaInvoc@ProgramInvocation {
    progInvokeArgs  = args
    } = javaInvoc {progInvokeArgs = args ++ proxySetting}
    where hasProxySetting = any (isSubsequenceOf "proxyHost") args
          proxySetting = if not hasProxySetting then
                           ["-Djava.net.useSystemProxies=true"]
                         else []

-- TODO: Extremely dirty (but safe) hack because etlas-cabal has no HTTP-aware modules.
--       Basically, we want to be able to search the index for a given eta version
--       when we can't find any. But we need HTTP ability for that.
findCoursierRef :: IORef (Verbosity -> NoCallStackIO ())
findCoursierRef = unsafePerformIO $ newIORef $ \verbosity -> do
  info verbosity $ "The Coursier Ref has not been initialized!"
  return ()

runCoursier :: Verbosity -> [String] -> ProgramDb -> IO String
runCoursier verbosity opts progDb = do
  etlasToolsDir <- defaultEtlasToolsDir
  let path = etlasToolsDir </> "coursier"
  exists <- doesFileExist path
  when (not exists) $ do
    findCoursier <- readIORef findCoursierRef
    findCoursier verbosity

  runJava verbosity ["-noverify"] (Jar path) opts progDb

findEtaServ :: Verbosity -> ProgramDb -> IO FilePath
findEtaServ _verbosity _progdb = do
  -- (javaProg,_) <- requireProgram verbosity javaProgram progdb
  etlasToolsDir <- defaultEtlasToolsDir
  return $ etlasToolsDir </> "eta-serv.jar"

-- TODO: Extremely dirty (but safe) hack because etlas-cabal has no HTTP-aware modules.
--       Basically, we want to be able to search the index for a given eta version
--       when we can't find any. But we need HTTP ability for that.
findVerifyRef :: IORef (Verbosity -> NoCallStackIO ())
findVerifyRef = unsafePerformIO $ newIORef $ \verbosity -> do
  info verbosity $ "The Verify Ref has not been initialized!"
  return ()

runVerify :: Verbosity -> [String] -> String -> LocalBuildInfo -> IO ()
runVerify verbosity classPath target lbi = do
  etlasToolsDir <- defaultEtlasToolsDir
  let path = etlasToolsDir </> "classes"
      verifyClass = path </> "Verify.class"
  exists <- doesFileExist verifyClass
  when (not exists) $ do
    findVerify <- readIORef findVerifyRef
    findVerify verbosity

  _ <- runJava verbosity ["-cp", mkMergedClassPath lbi (path:classPath)]
         (JavaClass "Verify") [target] (withPrograms lbi)
  return ()

fetchMavenDependencies :: Verbosity -> [String] -> [String] -> ProgramDb -> IO [String]
fetchMavenDependencies _ _ [] _ = return []
fetchMavenDependencies verb repos deps progDb = do
  let resolvedRepos = concatMap (\r -> ["-r", resolveOrId r]) repos
  fmap lines $ runCoursier verb (["fetch", "-a", "jar","--quiet"] ++ deps ++ resolvedRepos) progDb

getInstalledPackagesMonitorFiles :: Verbosity -> Platform
                                 -> ProgramDb
                                 -> [PackageDB]
                                 -> IO [FilePath]
getInstalledPackagesMonitorFiles verbosity platform progdb =
    traverse getPackageDBPath
  where
    getPackageDBPath :: PackageDB -> IO FilePath
    getPackageDBPath GlobalPackageDB =
      selectMonitorFile =<< getGlobalPackageDB verbosity etaProg

    -- For Eta, both Global and User are one and the same.
    getPackageDBPath UserPackageDB =
      selectMonitorFile =<< getGlobalPackageDB verbosity etaProg

    getPackageDBPath (SpecificPackageDB path) = selectMonitorFile path

    -- GHC has old style file dbs, and new style directory dbs.
    -- Note that for dir style dbs, we only need to monitor the cache file, not
    -- the whole directory. The ghc program itself only reads the cache file
    -- so it's safe to only monitor this one file.
    selectMonitorFile path = do
      isFileStyle <- doesFileExist path
      if isFileStyle then return path
                     else return (path </> "package.cache")

    Just etaProg = lookupProgram etaProgram progdb
