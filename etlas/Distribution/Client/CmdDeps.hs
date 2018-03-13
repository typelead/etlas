{-# LANGUAGE NamedFieldPuns #-}

-- | etlas CLI command: deps
--
module Distribution.Client.CmdDeps (
    -- * The @deps@ CLI and action
    depsCommand,
    depsAction,

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
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import Distribution.Client.DistDirLayout
import qualified Distribution.Client.IndexUtils as IndexUtils
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Simple.Compiler
         ( Compiler(..), PackageDB(..) )
import Distribution.Types.ComponentName
         ( showComponentName )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity, normal )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Utils
         ( wrapText, die', ordNub, info, notice, findFile )
import Distribution.Client.ProjectPlanning
         ( ElaboratedConfiguredPackage(..), ElaboratedSharedConfig(..),
           ElaboratedInstallPlan )
import Distribution.Client.InstallPlan
         ( toList, foldPlanPackage )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.PackageDescription
import Distribution.InstalledPackageInfo
import Distribution.Types.UnitId
         ( UnitId )
import Distribution.Types.ForeignLib

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import System.FilePath
import System.Directory hiding (findFile)
import Control.Monad


depsCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
depsCommand = Client.installCommand {
  commandName         = "deps",
  commandSynopsis     = "Return the list of all dependency jars and Maven dependencies for the specified targets.",
  commandUsage        = usageAlternatives "deps"
                          [ "[TARGET] [FLAGS] [-- EXECUTABLE_FLAGS]" ],
  commandDescription  = Just $ \pname -> wrapText $
  -- TODO: Update documentation
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
      -- TODO: Update documentation
        "Examples:\n"
     ++ "  " ++ pname ++ " deps\n"
     ++ "    Run the executable-like in the package in the current directory\n"
     ++ "  " ++ pname ++ " deps foo-tool\n"
     ++ "    Run the named executable-like (in any package in the project)\n"
     ++ "  " ++ pname ++ " deps pkgfoo:foo-tool\n"
     ++ "    Run the executable-like 'foo-tool' in the package 'pkgfoo'\n"
     ++ "  " ++ pname ++ " deps foo -O2 -- dothing --fooflag\n"
     ++ "    Build with '-O2' and run the program, passing it extra arguments.\n\n"

     ++ cmdCommonHelpTextNewBuildBeta
   }


-- | The @deps@ command build the specified component and returns a list of all
-- the jar files and maven dependencies. Maven dependencies are prefixed with
-- "maven:" and jar dependencies are prefixed with "file:".
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
depsAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
          -> [String] -> GlobalFlags -> IO ()
depsAction (configFlags, configExFlags, installFlags, haddockFlags)
            targetStrings globalFlags = do

    baseCtx <- establishProjectBaseContext verbosity cliConfig

    targetSelectors <- either (reportTargetSelectorProblems verbosity) return
                   =<< readTargetSelectors (localPackages baseCtx) targetStrings

    (buildCtx, pkg) <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do

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
            (selectedComponentId, _) <- singleExeOrElse
                   (reportTargetProblems
                     verbosity
                      [TargetProblemMultipleTargets targets])
                   targets

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionBuild
                                    targets
                                    elaboratedPlan

            let pkg = foldPlanPackage
                      (error $ "Selected component" ++ display selectedComponentId
                            ++ " is a pre-existing package instead of a local"
                            ++ " package.") id
                    . fromMaybe
                      (error $ "Selected component " ++ display selectedComponentId
                            ++ " not found in target-pruned elaborated install plan.")
                    $ InstallPlan.lookup elaboratedPlan' selectedComponentId

            -- The `deps` command forces the dependencies to be built so that it can
            -- provide results properly.

            elaboratedPlan'' <-
              either (reportCannotPruneDependencies verbosity) return $
                     pruneInstallPlanToDependencies (Map.keysSet targets)
                     elaboratedPlan'

            return (elaboratedPlan'', targets, pkg)

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

    -- -- In the common case, we expect @matchingElaboratedConfiguredPackages@
    -- -- to consist of a single element that provides a single way of building
    -- -- an appropriately-named executable. In that case we take that
    -- -- package and continue.
    -- --
    -- -- However, multiple packages/components could provide that
    -- -- executable, or it's possible we don't find the executable anywhere
    -- -- in the build plan. I suppose in principle it's also possible that
    -- -- a single package provides an executable in two different ways,
    -- -- though that's probably a bug if. Anyway it's a good lint to report
    -- -- an error in all of these cases, even if some seem like they
    -- -- shouldn't happen.
    -- pkg <- case matchingElaboratedConfiguredPackages of
    --   [] -> die' verbosity $ "Selected " ++ display selectedUnitId
    --   [elabPkg] -> do
    --     info verbosity $ "Selecting " ++ display selectedUnitId
    --     return elabPkg
    --   elabPkgs -> die' verbosity
    --     $ "Multiple matching components found:"
    --     ++ unlines (fmap (\p -> " - in package " ++ display (elabUnitId p)) elabPkgs)
    let elabShared = elaboratedShared buildCtx
        compiler   = pkgConfigCompiler      elabShared
        progdb     = pkgConfigCompilerProgs elabShared
        packageDBs' = Set.toList
                    . Set.fromList
                    $ elabBuildPackageDBStack pkg
                   ++ storePackageDBStack
                        (cabalStoreDirLayout (cabalDirLayout baseCtx))
                        (compilerId compiler)
    packageDBs <- fmap reverse
      $ foldM (\dirs dir ->
                 case dir of
                   SpecificPackageDB path -> do
                     exists <- doesDirectoryExist path
                     if exists
                     then return (dir:dirs)
                     else return dirs
                   _ -> return (dir:dirs)) [] packageDBs'

    packageIndex <- IndexUtils.getInstalledPackages verbosity compiler packageDBs
                      progdb

    let planPackageUnitIds =
          map (foldPlanPackage installedUnitId elabUnitId) $ toList elaboratedPlan
        packageInfos =
          catMaybes $ map (PackageIndex.lookupUnitId packageIndex) planPackageUnitIds

    when (length packageInfos /= length planPackageUnitIds) $ do
      die' verbosity $
           "There were packages in the elaborated install plan that were missing"
        ++ " from the installed package index."

    let transitiveMavenDeps =
          concatMap extraLibraries packageInfos
        componentMavenDeps =
          extraLibsComponent selectedComponent (elabPkgDescription pkg)
        allMavenDeps = componentMavenDeps ++ transitiveMavenDeps

    transitiveJars <- fmap concat $ mapM hsLibraryPaths packageInfos

    mapM_ (notice verbosity . ("file:" ++))  transitiveJars
    mapM_ (notice verbosity . ("maven:" ++)) allMavenDeps

  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags

    hsLibraryPaths pinfo =
      mapM (findFile (libraryDirs pinfo)) . map (<.> "jar") $ hsLibraries pinfo


singleExeOrElse :: IO (UnitId, ComponentName) -> TargetsMap
                -> IO (UnitId, ComponentName)
singleExeOrElse action targetsMap =
  case Set.toList . distinctTargetComponents $ targetsMap of
    [(unitId, componentName)] -> return (unitId, componentName)
    _          -> action

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

-- | Return the extraLibs field of the corresponding component.
extraLibsComponent :: ComponentName -> PackageDescription -> [String]
extraLibsComponent componentName pkgDescr =
  case componentName of
    CLibName -> extraLibs . libBuildInfo . fromJust $ library pkgDescr
    CSubLibName cn ->
      getExtraLibs cn libBuildInfo  (fromJust . libName) subLibraries
    CFLibName cn ->
      getExtraLibs cn foreignLibBuildInfo foreignLibName foreignLibs
    CExeName cn ->
      getExtraLibs cn buildInfo exeName executables
    CTestName cn ->
      getExtraLibs cn testBuildInfo testName testSuites
    CBenchName cn ->
      getExtraLibs cn benchmarkBuildInfo benchmarkName benchmarks
  where getExtraLibs compName biSelector cnSelector typeSelector =
          extraLibs . biSelector . head .
          filter ((== compName) . cnSelector) $ typeSelector pkgDescr


-- | This defines what a 'TargetSelector' means for the @deps@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @deps@ command we select the exe if there is only one and it's
-- buildable. Fail if there are no or multiple buildable exe components.
--
selectPackageTargets :: TargetSelector
                     -> [AvailableTarget k] -> Either TargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there is exactly one buildable executable then we select that
  | [target] <- targetsBuildable
  = Right [target]

    -- but fail if there are multiple buildable executables.
  | not (null targetsBuildable)
  = Left (TargetProblemMatchesMultiple targetSelector targetsBuildable')

    -- If there are components but none are buildable then we report those
  | not (null targets)
  = Left (TargetProblemNoneEnabled targetSelector targetsNoDetail)

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    (targetsBuildable, targetsBuildable') = selectBuildableTargets' targets
    targetsNoDetail = forgetTargetsDetail targets


-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @deps@ command we just need to check it is a executable, in addition
-- to the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem k
selectComponentTarget subtarget@WholeComponent t =
    either (Left . TargetProblemCommon) Right
  $ selectComponentTargetBasic subtarget t

selectComponentTarget subtarget t
  = Left (TargetProblemIsSubComponent (availableTargetPackageId t)
                                      (availableTargetComponentName t)
                                       subtarget)

-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @deps@ command.
--
data TargetProblem =
     TargetProblemCommon       TargetProblemCommon
     -- | The 'TargetSelector' matches targets but none are buildable
   | TargetProblemNoneEnabled TargetSelector [AvailableTarget ()]

     -- | There are no targets at all
   | TargetProblemNoTargets   TargetSelector

     -- | A single 'TargetSelector' matches multiple targets
   | TargetProblemMatchesMultiple TargetSelector [AvailableTarget ()]

     -- | Multiple 'TargetSelector's match multiple targets
   | TargetProblemMultipleTargets TargetsMap

     -- | Asking to run an individual file or module is not supported
   | TargetProblemIsSubComponent  PackageId ComponentName SubComponentTarget
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (TargetProblemCommon problem) =
    renderTargetProblemCommon "deps" problem

renderTargetProblem (TargetProblemNoneEnabled targetSelector targets) =
    renderTargetProblemNoneEnabled "deps" targetSelector targets

renderTargetProblem (TargetProblemNoTargets targetSelector) =
    renderTargetProblemNoTargets "deps" targetSelector

renderTargetProblem (TargetProblemMatchesMultiple targetSelector targets) =
    "The deps command is for fetching dependencies for a single component at once."
 ++ " The target'"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " which includes "
 ++ renderListCommaAnd ( ("the "++) <$>
                         showComponentName <$>
                         availableTargetComponentName <$>
                         targets )

renderTargetProblem (TargetProblemMultipleTargets selectorMap) =
    "The deps command is for fetching dependencies for a single component at once."
 ++ " The targets "
 ++ renderListCommaAnd [ "'" ++ showTargetSelector ts ++ "'"
                       | ts <- ordNub (concatMap snd (concat (Map.elems selectorMap))) ]
 ++ " refer to different executables."

renderTargetProblem (TargetProblemIsSubComponent pkgid cname subtarget) =
    "The deps command can only report dependencies for a component as a whole, "
 ++ "not files or modules within them, but the target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ "."
  where
    targetSelector = TargetComponent pkgid cname subtarget

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
    die' verbosity . renderCannotPruneDependencies
