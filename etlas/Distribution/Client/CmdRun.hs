{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

-- | etlas CLI command: run
--
module Distribution.Client.CmdRun (
    -- * The @run@ CLI and action
    runCommand,
    runAction,

    -- * Internals exposed for testing
    TargetProblem(..),
    selectPackageTargets,
    selectComponentTarget
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags, RunFlags(..),
           defaultRunFlags, liftOptions )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import qualified Distribution.Simple.Eta as Eta
import Distribution.Types.ComponentName
         ( showComponentName )
import Distribution.Text
         ( display )
import Distribution.System
         ( buildOS, OS(..) )
import Distribution.Verbosity
         ( Verbosity, normal )
import Distribution.Simple.Utils
         ( wrapText, die', ordNub, info )
import Distribution.Client.ProjectPlanning
         ( ElaboratedConfiguredPackage(..), BuildStyle(..), ElaboratedSharedConfig(..)
         , ElaboratedInstallPlan, binDirectoryFor )
import Distribution.Client.InstallPlan
         ( toList, foldPlanPackage )
import Distribution.Types.UnqualComponentName
         ( UnqualComponentName, unUnqualComponentName )
import Distribution.Types.PackageDescription
         ( PackageDescription(dataDir) )
import Distribution.Simple.Program.Run
         ( runProgramInvocation, ProgramInvocation(..),
           emptyProgramInvocation )
import Distribution.Simple.Build.PathsModule
         ( pkgPathEnvVar )
import Distribution.Types.UnitId
         ( UnitId )
import Distribution.Client.Types
         ( PackageLocation(..) )

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.FilePath
import Data.List

runCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, RunFlags)
runCommand = Client.installCommand {
  commandName         = "run",
  commandSynopsis     = "Run an executable.",
  commandUsage        = usageAlternatives "run"
                          [ "[TARGET] [FLAGS] [-- EXECUTABLE_FLAGS]" ],
  commandDescription  = Just $ \pname -> wrapText $
        "Runs the specified executable-like component (an executable, a test, "
     ++ "or a benchmark), first ensuring it is up to date.\n\n"

     ++ "Any executable-like component in any package in the project can be "
     ++ "specified. A package can be specified if contains just one "
     ++ "executable-like. The default is to use the package in the current "
     ++ "directory if it contains just one executable-like.\n\n"

     ++ "Extra arguments can be passed to the program, but use '--' to "
     ++ "separate arguments for the program from arguments for " ++ pname
     ++ ". The executable is run in an environment where it can find its "
     ++ "data files inplace in the build tree.\n\n"

     ++ "Dependencies are built or rebuilt as necessary. Additional "
     ++ "configuration flags can be specified on the command line and these "
     ++ "extend the project configuration from the 'cabal.project', "
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " run\n"
     ++ "    Run the executable-like in the package in the current directory\n"
     ++ "  " ++ pname ++ " run foo-tool\n"
     ++ "    Run the named executable-like (in any package in the project)\n"
     ++ "  " ++ pname ++ " run pkgfoo:foo-tool\n"
     ++ "    Run the executable-like 'foo-tool' in the package 'pkgfoo'\n"
     ++ "  " ++ pname ++ " run foo -O2 -- dothing --fooflag\n"
     ++ "    Build with '-O2' and run the program, passing it extra arguments.\n\n"

     ++ cmdCommonHelpTextNewBuildBeta,
  commandDefaultFlags = (f1, f2, f3, f4, defaultRunFlags),
  commandOptions = \showOrParseArgs ->
      liftOptions getInstallOpts setInstallOpts
        (commandOptions Client.installCommand showOrParseArgs)
   ++ liftOptions getRunOpts setRunOpts
        (Client.runOptions showOrParseArgs)
   }
  where getInstallOpts (a,b,c,d,_)           = (a,b,c,d)
        setInstallOpts (a,b,c,d) (_,_,_,_,e) = (a,b,c,d,e)
        getRunOpts (_,_,_,_,e)   = e
        setRunOpts e (a,b,c,d,_) = (a,b,c,d,e)
        (f1, f2, f3, f4) = commandDefaultFlags Client.installCommand


-- | The @run@ command runs a specified executable-like component, building it
-- first if necessary. The component can be either an executable, a test,
-- or a benchmark. This is particularly useful for passing arguments to
-- exes/tests/benchs by simply appending them after a @--@.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
runAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, RunFlags)
          -> [String] -> GlobalFlags -> IO ()
runAction (configFlags, configExFlags, installFlags, haddockFlags, runFlags)
            targetStrings globalFlags = do

    baseCtx <- establishProjectBaseContext verbosity cliConfig

    targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                   =<< readTargetSelectors (localPackages baseCtx)
                         (take 1 targetStrings) -- Drop the exe's args.

    (buildCtx, _) <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do

            when (buildSettingOnlyDeps (buildSettings baseCtx)) $
              die' verbosity $
                  "The run command does not support '--only-dependencies'. "
               ++ "You may wish to use 'build --only-dependencies' and then "
               ++ "use 'run'."

            -- Interpret the targets on the command line as build targets
            -- (as opposed to say repl or haddock targets).
            targets <- either (reportTargetProblems verbosity) return
                     $ resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         targetSelectors

            -- Reject multiple targets, or at least targets in different
            -- components. It is ok to have two module/file targets in the
            -- same component, but not two that live in different components.
            --
            -- Note that we discard the target and return the whole 'TargetsMap',
            -- so this check will be repeated (and must succeed) after
            -- the 'runProjectPreBuildPhase'. Keep it in mind when modifying this.
            _ <- singleExeOrElse
                   (reportTargetProblems
                      verbosity
                      [TargetProblemMultipleTargets targets])
                   targets

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan
            return (elaboratedPlan', targets, ())

    (selectedUnitId, selectedComponent) <-
      -- Slight duplication with 'runProjectPreBuildPhase'.
      singleExeOrElse
        (die' verbosity $ "No or multiple targets given, but the run "
                       ++ "phase has been reached. This is a bug.")
        $ targetsMap buildCtx

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes


    let elaboratedPlan = elaboratedPlanToExecute buildCtx
        matchingElaboratedConfiguredPackages =
          matchingPackagesByUnitId
            selectedUnitId
            elaboratedPlan

    let exeName = unUnqualComponentName selectedComponent

    -- In the common case, we expect @matchingElaboratedConfiguredPackages@
    -- to consist of a single element that provides a single way of building
    -- an appropriately-named executable. In that case we take that
    -- package and continue.
    --
    -- However, multiple packages/components could provide that
    -- executable, or it's possible we don't find the executable anywhere
    -- in the build plan. I suppose in principle it's also possible that
    -- a single package provides an executable in two different ways,
    -- though that's probably a bug if. Anyway it's a good lint to report
    -- an error in all of these cases, even if some seem like they
    -- shouldn't happen.
    pkg <- case matchingElaboratedConfiguredPackages of
      [] -> die' verbosity $ "Unknown executable "
                          ++ exeName
                          ++ " in package "
                          ++ display selectedUnitId
      [elabPkg] -> do
        info verbosity $ "Selecting "
                       ++ display selectedUnitId
                       ++ " to supply " ++ exeName
        return elabPkg
      elabPkgs -> die' verbosity
        $ "Multiple matching executables found matching "
        ++ exeName
        ++ ":\n"
        ++ unlines (fmap (\p -> " - in package " ++ display (elabUnitId p)) elabPkgs)
    let exeNameExt = case buildOS of
                       Windows -> exeName <.> "cmd"
                       _       -> exeName
        exePath = binDirectoryFor (distDirLayout baseCtx)
                                  (elaboratedShared buildCtx)
                                  pkg
                                  exeName
               </> exeNameExt

        ElaboratedSharedConfig {..} = elaboratedShared buildCtx

        envModDebug
            | debug = [("ETA_JAVA_CMD", Just "jdb")]
            | otherwise = []
        envModTrace
          | trace = do
              mavenDeps <- Eta.fetchMavenDependencies verbosity []
                             [ "org.slf4j:slf4j-ext:1.7.21"
                             , "org.slf4j:slf4j-simple:1.7.21"
                             , "org.slf4j:slf4j-api:1.7.21"
                             , "org.javassist:javassist:3.20.0-GA"]
                             pkgConfigCompilerProgs
              let (agent:[], classpaths) =
                    partition ("slf4j-ext" `isInfixOf`) mavenDeps
                  javaArgs =
                       "-Djava.compiler=NONE -javaagent:"
                    ++ agent
                    ++ "=ignore=org/slf4j/:ch/qos/logback/:org/apache/log4j/:eta/runtime/stg/Print"
                    ++ traceIgnore
                  etaClasspath = Eta.mkMergedClassPath pkgConfigPlatform classpaths
              return [("JAVA_ARGS", Just javaArgs), ("ETA_CLASSPATH", Just etaClasspath)]
          | otherwise = return []
    envChanges <- fmap (envModDebug ++) $ envModTrace
    let args = drop 1 targetStrings
    runProgramInvocation
      verbosity
      emptyProgramInvocation {
        progInvokePath  = exePath,
        progInvokeArgs  = args,
        progInvokeEnv   = dataDirsEnvironmentForPlan elaboratedPlan ++ envChanges
      }
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags

    debug       = fromFlagOrDefault False (runDebug runFlags)
    trace       = fromFlagOrDefault False (runTrace runFlags)
    traceIgnore = maybe "" (':' :) $ flagToMaybe (runTraceIgnore runFlags)


-- | Construct the environment needed for the data files to work.
-- This consists of a separate @*_datadir@ variable for each
-- inplace package in the plan.
dataDirsEnvironmentForPlan :: ElaboratedInstallPlan
                           -> [(String, Maybe FilePath)]
dataDirsEnvironmentForPlan = catMaybes
                           . fmap (foldPlanPackage
                               (const Nothing)
                               dataDirEnvVarForPackage)
                           . toList

-- | Construct an environment variable that points
-- the package's datadir to its correct location.
-- This might be:
-- * 'Just' the package's source directory plus the data subdirectory
--   for inplace packages.
-- * 'Nothing' for packages installed in the store (the path was
--   already included in the package at install/build time).
-- * The other cases are not handled yet. See below.
dataDirEnvVarForPackage :: ElaboratedConfiguredPackage
                        -> Maybe (String, Maybe FilePath)
dataDirEnvVarForPackage pkg =
  case (elabBuildStyle pkg, elabPkgSourceLocation pkg)
  of (BuildAndInstall, _) -> Nothing
     (BuildInplaceOnly, LocalUnpackedPackage path) -> Just
       (pkgPathEnvVar (elabPkgDescription pkg) "datadir",
        Just $ path </> dataDir (elabPkgDescription pkg))
     -- TODO: handle the other cases for PackageLocation.
     -- We will only need this when we add support for
     -- remote/local tarballs.
     (BuildInplaceOnly, _) -> Nothing

singleExeOrElse :: IO (UnitId, UnqualComponentName) -> TargetsMap -> IO (UnitId, UnqualComponentName)
singleExeOrElse action targetsMap =
  case Set.toList . distinctTargetComponents $ targetsMap
  of [(unitId, CExeName component)] -> return (unitId, component)
     [(unitId, CTestName component)] -> return (unitId, component)
     [(unitId, CBenchName component)] -> return (unitId, component)
     _   -> action

-- | Filter the 'ElaboratedInstallPlan' keeping only the
-- 'ElaboratedConfiguredPackage's that match the specified
-- 'UnitId'.
matchingPackagesByUnitId :: UnitId
                         -> ElaboratedInstallPlan
                         -> [ElaboratedConfiguredPackage]
matchingPackagesByUnitId uid =
          catMaybes
          . fmap (foldPlanPackage
                    (const Nothing)
                    (\x -> if elabUnitId x == uid
                           then Just x
                           else Nothing))
          . toList

-- | This defines what a 'TargetSelector' means for the @run@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @run@ command we select the exe if there is only one and it's
-- buildable. Fail if there are no or multiple buildable exe components.
--
selectPackageTargets :: TargetSelector
                     -> [AvailableTarget k] -> Either TargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there is exactly one buildable executable then we select that
  | [target] <- targetsExesBuildable
  = Right [target]

    -- but fail if there are multiple buildable executables.
  | not (null targetsExesBuildable)
  = Left (TargetProblemMatchesMultiple targetSelector targetsExesBuildable')

    -- If there are executables but none are buildable then we report those
  | not (null targetsExes)
  = Left (TargetProblemNoneEnabled targetSelector targetsExes)

    -- If there are no executables but some other targets then we report that
  | not (null targets)
  = Left (TargetProblemNoExes targetSelector)

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    -- Targets that can be executed
    targetsExecutableLike =
      concatMap (\kind -> filterTargetsKind kind targets)
                [ExeKind, TestKind, BenchKind]
    (targetsExesBuildable,
     targetsExesBuildable') = selectBuildableTargets' targetsExecutableLike

    targetsExes             = forgetTargetsDetail targetsExecutableLike


-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @run@ command we just need to check it is a executable, in addition
-- to the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem  k
selectComponentTarget subtarget@WholeComponent t
  = case availableTargetComponentName t
    of CExeName _ -> component
       CTestName _ -> component
       CBenchName _ -> component
       _ -> Left (TargetProblemComponentNotExe pkgid cname)
    where pkgid = availableTargetPackageId t
          cname = availableTargetComponentName t
          component = either (Left . TargetProblemCommon) return $
                        selectComponentTargetBasic subtarget t

selectComponentTarget subtarget t
  = Left (TargetProblemIsSubComponent (availableTargetPackageId t)
                                      (availableTargetComponentName t)
                                       subtarget)

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @run@ command.
--
data TargetProblem =
     TargetProblemCommon       TargetProblemCommon
     -- | The 'TargetSelector' matches targets but none are buildable
   | TargetProblemNoneEnabled TargetSelector [AvailableTarget ()]

     -- | There are no targets at all
   | TargetProblemNoTargets   TargetSelector

     -- | The 'TargetSelector' matches targets but no executables
   | TargetProblemNoExes      TargetSelector

     -- | A single 'TargetSelector' matches multiple targets
   | TargetProblemMatchesMultiple TargetSelector [AvailableTarget ()]

     -- | Multiple 'TargetSelector's match multiple targets
   | TargetProblemMultipleTargets TargetsMap

     -- | The 'TargetSelector' refers to a component that is not an executable
   | TargetProblemComponentNotExe PackageId ComponentName

     -- | Asking to run an individual file or module is not supported
   | TargetProblemIsSubComponent  PackageId ComponentName SubComponentTarget
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (TargetProblemCommon problem) =
    renderTargetProblemCommon "run" problem

renderTargetProblem (TargetProblemNoneEnabled targetSelector targets) =
    renderTargetProblemNoneEnabled "run" targetSelector targets

renderTargetProblem (TargetProblemNoExes targetSelector) =
    "Cannot run the target '" ++ showTargetSelector targetSelector
 ++ "' which refers to " ++ renderTargetSelector targetSelector
 ++ " because "
 ++ plural (targetSelectorPluralPkgs targetSelector) "it does" "they do"
 ++ " not contain any executables."

renderTargetProblem (TargetProblemNoTargets targetSelector) =
    case targetSelectorFilter targetSelector of
      Just kind | kind /= ExeKind
        -> "The run command is for running executables, but the target '"
           ++ showTargetSelector targetSelector ++ "' refers to "
           ++ renderTargetSelector targetSelector ++ "."

      _ -> renderTargetProblemNoTargets "run" targetSelector

renderTargetProblem (TargetProblemMatchesMultiple targetSelector targets) =
    "The run command is for running a single executable at once. The target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " which includes "
 ++ renderListCommaAnd ( ("the "++) <$>
                         showComponentName <$>
                         availableTargetComponentName <$>
                         foldMap
                           (\kind -> filterTargetsKind kind targets)
                           [ExeKind, TestKind, BenchKind] )

renderTargetProblem (TargetProblemMultipleTargets selectorMap) =
    "The run command is for running a single executable at once. The targets "
 ++ renderListCommaAnd [ "'" ++ showTargetSelector ts ++ "'"
                       | ts <- ordNub (concatMap snd (concat (Map.elems selectorMap))) ]
 ++ " refer to different executables."

renderTargetProblem (TargetProblemComponentNotExe pkgid cname) =
    "The run command is for running executables, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " from the package "
 ++ display pkgid ++ "."
  where
    targetSelector = TargetComponent pkgid cname WholeComponent

renderTargetProblem (TargetProblemIsSubComponent pkgid cname subtarget) =
    "The run command can only run an executable as a whole, "
 ++ "not files or modules within them, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ "."
  where
    targetSelector = TargetComponent pkgid cname subtarget
