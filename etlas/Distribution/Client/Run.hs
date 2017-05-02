-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Run
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'run' command.
-----------------------------------------------------------------------------
{-# LANGUAGE MultiWayIf #-}

module Distribution.Client.Run ( run, splitRunArgs )
       where

import Prelude ()
import Data.List
import Data.Maybe (fromMaybe)
import Distribution.Client.Compat.Prelude

import Distribution.Types.TargetInfo     (targetCLBI)
import Distribution.Types.LocalBuildInfo (componentNameTargets')

import Distribution.Client.Utils             (tryCanonicalizePath)

import Distribution.Types.UnqualComponentName
import Distribution.PackageDescription       (Executable (..),
                                              TestSuite(..),
                                              Benchmark(..),
                                              PackageDescription (..),
                                              BuildInfo(buildable))
import Distribution.Simple.Compiler          (compilerFlavor, CompilerFlavor(..))
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Distribution.Simple.BuildPaths        (exeExtension)
import Distribution.Simple.LocalBuildInfo    (ComponentName (..),
                                              LocalBuildInfo (..),
                                              depLibraryPaths)
import Distribution.Simple.Utils             (die', notice, warn,
                                              rawSystemExitWithEnv,
                                              addLibraryPath)
import Distribution.System                   (Platform (..), OS(..))
import Distribution.Verbosity                (Verbosity)
import Distribution.Text                     (display)

import Distribution.Simple.Program
import Distribution.Client.Config

import qualified Distribution.Simple.Eta as Eta
import qualified Distribution.Simple.GHCJS as GHCJS

import System.Directory                      (getCurrentDirectory)
import Distribution.Compat.Environment       (getEnvironment)
import System.FilePath                       ((<.>), (</>), takeDirectory)


-- | Return the executable to run and any extra arguments that should be
-- forwarded to it. Die in case of error.
splitRunArgs :: Verbosity -> LocalBuildInfo -> [String]
             -> IO (Executable, [String])
splitRunArgs verbosity lbi args =
  case whichExecutable of -- Either err (wasManuallyChosen, exe, paramsRest)
    Left err               -> do
      warn verbosity `traverse_` maybeWarning -- If there is a warning, print it.
      die' verbosity err
    Right (True, exe, xs)  -> return (exe, xs)
    Right (False, exe, xs) -> do
      let addition = " Interpreting all parameters to `run` as a parameter to"
                     ++ " the default executable."
      -- If there is a warning, print it together with the addition.
      warn verbosity `traverse_` fmap (++addition) maybeWarning
      return (exe, xs)
  where
    pkg_descr = localPkgDescr lbi
    whichExecutable :: Either String       -- Error string.
                              ( Bool       -- If it was manually chosen.
                              , Executable -- The executable.
                              , [String]   -- The remaining parameters.
                              )
    whichExecutable = case (enabledExes, args) of
      ([]   , _)           -> Left "Couldn't find any enabled executables."
      ([exe], [])          -> return (False, exe, [])
      ([exe], (x:xs))
        | x == unUnqualComponentName (exeName exe) -> return (True, exe, xs)
        | otherwise                                -> return (False, exe, args)
      (_    , [])                                  -> Left
        $ "This package contains multiple executables. "
        ++ "You must pass the executable name as the first argument "
        ++ "to 'etlas run'."
      (_    , (x:xs))      ->
        case find (\exe -> unUnqualComponentName (exeName exe) == x) enabledExes of
          Nothing  -> Left $ "No executable named '" ++ x ++ "'."
          Just exe -> return (True, exe, xs)
      where
        enabledExes = filter (buildable . buildInfo) (executables pkg_descr)

    maybeWarning :: Maybe String
    maybeWarning = case args of
      []    -> Nothing
      (x:_) -> lookup (mkUnqualComponentName x) components
      where
        components :: [(UnqualComponentName, String)] -- Component name, message.
        components =
          [ (name, "The executable '" ++ display name ++ "' is disabled.")
          | e <- executables pkg_descr
          , not . buildable . buildInfo $ e, let name = exeName e]

          ++ [ (name, "There is a test-suite '" ++ display name ++ "',"
                      ++ " but the `run` command is only for executables.")
             | t <- testSuites pkg_descr
             , let name = testName t]

          ++ [ (name, "There is a benchmark '" ++ display name ++ "',"
                      ++ " but the `run` command is only for executables.")
             | b <- benchmarks pkg_descr
             , let name = benchmarkName b]

-- | Run a given executable.
run :: Verbosity -> Bool -> Bool -> LocalBuildInfo -> Executable -> [String] -> IO ()
run verbosity debug trace lbi exe exeArgs = do
  curDir <- getCurrentDirectory
  let buildPref     = buildDir lbi
      pkg_descr     = localPkgDescr lbi
      dataDirEnvVar = (pkgPathEnvVar pkg_descr "datadir",
                       curDir </> dataDir pkg_descr)

  (path, runArgs) <-
    let exeName' = display $ exeName exe
    in case compilerFlavor (compiler lbi) of
      Eta -> do
         let exeFileExt  = if isWindows lbi then ".cmd" else ""
             exeFileName = exeName' ++ exeFileExt
         let dirEnvVarRef = if isWindows lbi then "%DIR%" else "$DIR"
             allArgs = if isWindows lbi then "%*" else "\"$@\""
             replace from to string
               | from `isPrefixOf` string
               = to ++ replace from to (drop (length from) string)
               | c:rest <- string
               = c : replace from to rest
               | otherwise = []
             replaceDir dir cmd = replace dirEnvVarRef dir cmd
             replaceArgs cmd = replace allArgs "" cmd
         p <- tryCanonicalizePath $
            buildPref </> exeName' </> exeFileName

         let exeDir = takeDirectory p
             getJavaCmd = fmap (head . drop 2 . lines) $ readFile p
         if | debug -> do
                javaCmd' <- getJavaCmd
                let javaCmd = replaceArgs $ replaceDir exeDir javaCmd'
                    (_, cmdArgs) = break (== ' ') javaCmd
                -- TODO: Does this work with spaces in cmdArgs classpath?
                return ("jdb", words cmdArgs)
            | trace -> do
                javaCmd'' <- getJavaCmd
                etlasDir <- defaultCabalDir
                (javaProg, _) <- requireProgram verbosity javaProgram (withPrograms lbi)
                let javaCmd' = replaceArgs $ replaceDir exeDir javaCmd''
                    coursierPath        = etlasDir </> "coursier"
                    runCoursier options = getProgramInvocationOutput verbosity
                                            (programInvocation javaProg $
                                              (["-jar", "-noverify", coursierPath] ++ options))
                    getTracePaths = do
                      output <- runCoursier ["fetch", "--quiet"
                                                    , "org.slf4j:slf4j-ext:1.7.21"
                                                    , "org.slf4j:slf4j-simple:1.7.21"
                                                    , "org.slf4j:slf4j-api:1.7.21"
                                                    , "org.javassist:javassist:3.20.0-GA"]
                      let (agent:[], classpaths) = partition ("slf4j-ext" `isInfixOf`) $
                                                   lines output
                      return (agent, classpaths)
                (agent, classpaths) <- getTracePaths
                let javaCmd =
                        replace "\" eta.main" " eta.main"
                      . replace "-classpath \""
                      ( "-Djava.compiler=NONE "
                     ++ "-javaagent:" ++ agent
                     ++ "=ignore=org/slf4j/:ch/qos/logback/:org/apache/log4j/:cern/colt/ "
                     ++ "-classpath "
                     ++ Eta.mkMergedClassPath lbi classpaths
                     ++ Eta.classPathSeparator lbi )
                      $ javaCmd'
                    cmd:args = words javaCmd
                return (cmd, args)
            | otherwise -> return (p, [])
      GHCJS -> do
        let (script, cmd, cmdArgs) =
              GHCJS.runCmd (withPrograms lbi)
                           (buildPref </> exeName' </> exeName')
        script' <- tryCanonicalizePath script
        return (cmd, cmdArgs ++ [script'])
      _     -> do
         p <- tryCanonicalizePath $
            buildPref </> exeName' </> (exeName' <.> exeExtension)
         return (p, [])

  env  <- (dataDirEnvVar:) <$> getEnvironment
  -- Add (DY)LD_LIBRARY_PATH if needed
  env' <- if withDynExe lbi
             then do let (Platform _ os) = hostPlatform lbi
                     clbi <- case componentNameTargets' pkg_descr lbi (CExeName (exeName exe)) of
                                [target] -> return (targetCLBI target)
                                [] -> die' verbosity "run: Could not find executable in LocalBuildInfo"
                                _ -> die' verbosity "run: Found multiple matching exes in LocalBuildInfo"
                     paths <- depLibraryPaths True False lbi clbi
                     return (addLibraryPath os paths env)
             else return env
  notice verbosity $ "Running " ++ display (exeName exe) ++ "..."
  rawSystemExitWithEnv verbosity path (runArgs++exeArgs) env'

isWindows :: LocalBuildInfo -> Bool
isWindows lbi | Platform _ Windows <- hostPlatform lbi = True
              | otherwise = False
