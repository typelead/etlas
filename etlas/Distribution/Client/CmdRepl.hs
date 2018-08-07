{-# LANGUAGE NamedFieldPuns #-}

-- | etlas CLI command: repl
--
module Distribution.Client.CmdRepl (
    -- * The @repl@ CLI and action
    replCommand,
    replAction,

    -- * Internals exposed for testing
    TargetProblem(..),
    selectPackageTargets,
    selectComponentTarget
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages
import Distribution.Client.RebuildMonad

import Distribution.Client.ProjectConfig
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags(..), InstallFlags )
import Distribution.Client.DistDirLayout
import Distribution.Client.Types
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Simple.InstallDirs
import Distribution.Solver.Types.SourcePackage
import Distribution.Package
import Distribution.PackageDescription hiding (packageDescription)
import Distribution.PackageDescription.PrettyPrint
import Distribution.Types.Dependency
import Distribution.Types.ComponentName
         ( componentNameString )
import Distribution.Text
import Distribution.Compiler
import Distribution.License
import Language.Haskell.Extension
import Distribution.Verbosity
         ( Verbosity, normal, silent )
import Distribution.Version
import Distribution.Utils.Generic
import Distribution.Simple.Utils
         ( die', withTempDirectory, createDirectoryIfMissingVerbose )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid ((<>))
import Data.Maybe
import Control.Monad (when)
import Control.Exception
import System.FilePath
import System.Directory


replCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
replCommand = Client.installCommand {
  commandName         = "repl",
  commandSynopsis     = "Open an interactive session for the given component.",
  commandUsage        = usageAlternatives "repl" [ "[TARGET] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Open an interactive session for a component within the project. The "
     ++ "available targets are the same as for the 'build' command: "
     ++ "individual components within packages in the project, including "
     ++ "libraries, executables, test-suites or benchmarks. Packages can "
     ++ "also be specified in which case the library component in the "
     ++ "package will be used, or the (first listed) executable in the "
     ++ "package if there is no library.\n\n"

     ++ "Dependencies are built or rebuilt as necessary. Additional "
     ++ "configuration flags can be specified on the command line and these "
     ++ "extend the project configuration from the 'cabal.project', "
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples, open an interactive session:\n"
     ++ "  " ++ pname ++ " repl\n"
     ++ "    for the default component in the package in the current directory\n"
     ++ "  " ++ pname ++ " repl pkgname\n"
     ++ "    for the default component in the package named 'pkgname'\n"
     ++ "  " ++ pname ++ " repl ./pkgfoo\n"
     ++ "    for the default component in the package in the ./pkgfoo directory\n"
     ++ "  " ++ pname ++ " repl cname\n"
     ++ "    for the component named 'cname'\n"
     ++ "  " ++ pname ++ " repl pkgname:cname\n"
     ++ "    for the component 'cname' in the package 'pkgname'\n\n"

     ++ cmdCommonHelpTextNewBuildBeta
   }


-- | The @repl@ command is very much like @build@. It brings the install plan
-- up to date, selects that part of the plan needed by the given or implicit
-- repl target and then executes the plan.
--
-- Compared to @build@ the difference is that only one target is allowed
-- (given or implicit) and the target type is repl rather than build. The
-- general plan execution infrastructure handles both build and repl targets.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
replAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
           -> [String] -> GlobalFlags -> IO ()
replAction (configFlags, configExFlags, installFlags, haddockFlags)
           targetStrings globalFlags = do

  globalTmp <- getTemporaryDirectory

  withTempDirectory verbosity' globalTmp "repl" $ \tmpDir -> do

    (baseCtx, targetSelectors, verbosity) <- maybeGlobalEnvironment tmpDir

    (buildCtx, _) <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do

            when (buildSettingOnlyDeps (buildSettings baseCtx)) $
              die' verbosity $ "The repl command does not support '--only-dependencies'. "
                 ++ "You may wish to use 'build --only-dependencies' and then "
                 ++ "use 'repl'."

            -- Interpret the targets on the command line as repl targets
            -- (as opposed to say build or haddock targets).
            targets <- either (reportTargetProblems verbosity) return
                     $ resolveTargets
                         selectPackageTargets
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         Nothing
                         targetSelectors

            -- Reject multiple targets, or at least targets in different
            -- components. It is ok to have two module/file targets in the
            -- same component, but not two that live in different components.
            when (Set.size (distinctTargetComponents targets) > 1) $
              reportTargetProblems verbosity
                [TargetProblemMultipleTargets targets]

            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionRepl
                                    targets
                                    elaboratedPlan
            return (elaboratedPlan', targets, ())

    printPlan verbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes
  where
    verbosity' = fromFlagOrDefault normal (configVerbosity configFlags)
    globalReplVerbosity = fromFlagOrDefault silent (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags

    maybeGlobalEnvironment tmpDir = do
      catch (do baseCtx <- establishProjectBaseContext verbosity' cliConfig
                eSelectors <- readTargetSelectors (localPackages baseCtx) (Just LibKind) targetStrings
                either (reportTargetSelectorProblems verbosity')
                       (\selectors -> return (baseCtx, selectors, verbosity'))
                       eSelectors)
            (\e@(BadPackageLocations provenance locations) -> do
                let isEmptyMatch =
                      any (\s -> case s of
                            BadLocGlobEmptyMatch _ -> True
                            _ -> False) locations
                if Set.singleton Implicit == provenance && isEmptyMatch
                then tryEstablishGlobal tmpDir
                else throwIO e)

    tryEstablishGlobal tmpDir = do
        let replVerbosity
              | null targetStrings = globalReplVerbosity
              | otherwise = verbosity'
        baseCtx <- establishDummyProjectBaseContext replVerbosity cliConfig
                     tmpDir targetStrings
        return (baseCtx, [TargetPackageNamed (mkPackageName "repl") Nothing],
                replVerbosity)

-- | Create a dummy project context, without a .cabal or a .cabal.project file
-- (a place where to put a temporary dist directory is still needed)
establishDummyProjectBaseContext
  :: Verbosity
  -> ProjectConfig
  -> FilePath
     -- ^ Where to put the dist directory
  -> [String]
  -> IO ProjectBaseContext
establishDummyProjectBaseContext verbosity cliConfig tmpDir targetStrings = do

    cabalDir <- defaultEtlasDir

    -- Create the dist directories
    createDirectoryIfMissingVerbose verbosity True $ distDirectory distDirLayout
    createDirectoryIfMissingVerbose verbosity True $
      distProjectCacheDirectory distDirLayout

    globalConfig <- runRebuild ""
                  $ readGlobalConfig verbosity
                      (projectConfigConfigFile (projectConfigShared cliConfig))
                      (projectConfigSendMetrics (projectConfigBuildOnly cliConfig))

    let projectConfig = globalConfig <> cliConfig

        ProjectConfigBuildOnly {
          projectConfigLogsDir,
          projectConfigStoreDir
        } = projectConfigBuildOnly projectConfig

        mlogsDir = flagToMaybe projectConfigLogsDir
        mstoreDir = flagToMaybe projectConfigStoreDir
        cabalDirLayout = mkCabalDirLayout cabalDir mstoreDir mlogsDir

        buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          projectConfig

        pkgdesc = GenericPackageDescription pkg []
                    (Just (CondNode { condTreeData        = library'
                                    , condTreeConstraints = dependencies
                                    , condTreeComponents  = []
                                    })) [] [] [] [] []
        library' = mempty {
                     libBuildInfo = mempty {
                         targetBuildDepends = dependencies,
                         defaultLanguage    = Just Haskell2010,
                         defaultExtensions  = [EnableExtension TemplateHaskell
                                              ,EnableExtension QuasiQuotes],
                         options            = [(Eta, ["-v1"])]
                     }
                   }
        mDepIds = map (\s -> (s, simpleParse s)) ("base":"eta-meta":targetStrings)
        badDepIds = filter (isNothing . snd) mDepIds
        dependencies = map (toDependency . fromJust . snd) mDepIds
        toDependency pkgid = Dependency (pkgName pkgid) versionRange
          where version = pkgVersion pkgid
                versionRange
                  | version == nullVersion = anyVersion
                  | otherwise = thisVersion version

        pkg = emptyPackageDescription {
                package        = PackageIdentifier (mkPackageName "repl")
                                                   (mkVersion [0]),
                specVersionRaw = Right (orLaterVersion (mkVersion [1,10])),
                buildType      = Just Simple,
                license        = OtherLicense,
                library        = Just library'
              }
        localPackages = [SpecificSourcePackage SourcePackage {
                           packageInfoId        = packageId pkgdesc,
                           packageDescription   = pkgdesc,
                           packageSource        = LocalUnpackedPackage tmpDir,
                           packageDescrOverride = Nothing,
                           packagePatch         = Nothing
                         }]

    when (not (null badDepIds)) $
      die' verbosity $
           "The following supplied arguments were invalid package identifiers: "
        ++ intercalate ", " (map fst badDepIds)

    writeGenericPackageDescription (tmpDir </> "repl.cabal") pkgdesc

    return ProjectBaseContext {
      distDirLayout,
      cabalDirLayout,
      projectConfig,
      localPackages,
      buildSettings
    }
  where
    mdistDirectory = flagToMaybe
                   $ projectConfigDistDir
                   $ projectConfigShared cliConfig
    projectRoot = ProjectRootImplicit tmpDir
    distDirLayout = defaultDistDirLayout projectRoot
                                         mdistDirectory

-- | This defines what a 'TargetSelector' means for the @repl@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For repl we select:
--
-- * the library if there is only one and it's buildable; or
--
-- * the exe if there is only one and it's buildable; or
--
-- * any other buildable component.
--
-- Fail if there are no buildable lib\/exe components, or if there are
-- multiple libs or exes.
--
selectPackageTargets  :: TargetSelector
                      -> [AvailableTarget k] -> Either TargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there is exactly one buildable library then we select that
  | [target] <- targetsLibsBuildable
  = Right [target]

    -- but fail if there are multiple buildable libraries.
  | not (null targetsLibsBuildable)
  = Left (TargetProblemMatchesMultiple targetSelector targetsLibsBuildable')

    -- If there is exactly one buildable executable then we select that
  | [target] <- targetsExesBuildable
  = Right [target]

    -- but fail if there are multiple buildable executables.
  | not (null targetsExesBuildable)
  = Left (TargetProblemMatchesMultiple targetSelector targetsExesBuildable')

    -- If there is exactly one other target then we select that
  | [target] <- targetsBuildable
  = Right [target]

    -- but fail if there are multiple such targets
  | not (null targetsBuildable)
  = Left (TargetProblemMatchesMultiple targetSelector targetsBuildable')

    -- If there are targets but none are buildable then we report those
  | not (null targets)
  = Left (TargetProblemNoneEnabled targetSelector targets')

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    targets'                = forgetTargetsDetail targets
    (targetsLibsBuildable,
     targetsLibsBuildable') = selectBuildableTargets'
                            . filterTargetsKind LibKind
                            $ targets
    (targetsExesBuildable,
     targetsExesBuildable') = selectBuildableTargets'
                            . filterTargetsKind ExeKind
                            $ targets
    (targetsBuildable,
     targetsBuildable')     = selectBuildableTargetsWith'
                                (isRequested targetSelector) targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    isRequested (TargetAllPackages  Nothing) TargetNotRequestedByDefault = False
    isRequested (TargetPackage _ _  Nothing) TargetNotRequestedByDefault = False
    isRequested _ _ = True


-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @repl@ command we just need the basic checks on being buildable etc.
--
selectComponentTarget :: SubComponentTarget
                      -> AvailableTarget k -> Either TargetProblem k
selectComponentTarget subtarget =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic subtarget


-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @repl@ command.
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
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (TargetProblemCommon problem) =
    renderTargetProblemCommon "open a repl for" problem

renderTargetProblem (TargetProblemMatchesMultiple targetSelector targets) =
    "Cannot open a repl for multiple components at once. The target '"
 ++ showTargetSelector targetSelector ++ "' refers to "
 ++ renderTargetSelector targetSelector ++ " which "
 ++ (if targetSelectorRefersToPkgs targetSelector then "includes " else "are ")
 ++ renderListSemiAnd
      [ "the " ++ renderComponentKind Plural ckind ++ " " ++
        renderListCommaAnd
          [ maybe (display pkgname) display (componentNameString cname)
          | t <- ts
          , let cname   = availableTargetComponentName t
                pkgname = packageName (availableTargetPackageId t)
          ]
      | (ckind, ts) <- sortGroupOn availableTargetComponentKind targets
      ]
 ++ ".\n\n" ++ explanationSingleComponentLimitation
  where
    availableTargetComponentKind = componentKind
                                 . availableTargetComponentName

renderTargetProblem (TargetProblemMultipleTargets selectorMap) =
    "Cannot open a repl for multiple components at once. The targets "
 ++ renderListCommaAnd
      [ "'" ++ showTargetSelector ts ++ "'"
      | ts <- ordNub (concatMap snd (concat (Map.elems selectorMap))) ]
 ++ " refer to different components."
 ++ ".\n\n" ++ explanationSingleComponentLimitation

renderTargetProblem (TargetProblemNoneEnabled targetSelector targets) =
    renderTargetProblemNoneEnabled "open a repl for" targetSelector targets

renderTargetProblem (TargetProblemNoTargets targetSelector) =
    renderTargetProblemNoTargets "open a repl for" targetSelector


explanationSingleComponentLimitation :: String
explanationSingleComponentLimitation =
    "The reason for this limitation is that current versions of Eta REPL do not "
 ++ "support loading multiple components as source. Load just one component "
 ++ "and when you make changes to a dependent component then quit and reload."
