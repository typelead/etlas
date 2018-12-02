{-# LANGUAGE CPP #-}
module Distribution.Client.PackageDescription.Dhall where

import Data.Function ( (&) )

import qualified Data.Text as StrictText
import qualified Data.Text.IO as StrictText

import qualified Dhall
import DhallToCabal (dhallToCabal)

import Distribution.Verbosity
import Distribution.PackageDescription.PrettyPrint
       (writeGenericPackageDescription)
#ifdef CABAL_PARSEC
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Distribution.PackageDescription.Parsec as Cabal.Parse
       (readGenericPackageDescription, parseGenericPackageDescriptionMaybe) 
#else
import Distribution.PackageDescription.Parse as Cabal.Parse
       (readGenericPackageDescription , parseGenericPackageDescription, ParseResult(..))
#endif
import Distribution.Simple.Utils (die', info, createDirectoryIfMissingVerbose)
import Distribution.PackageDescription
import Distribution.Types.Dependency
import Distribution.Types.ForeignLib
import Distribution.Types.UnqualComponentName
import Distribution.Types.CondTree

import qualified Lens.Micro as Lens
import Lens.Micro (Lens')
import qualified Lens.Micro.Extras as Lens

import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.CPUTime (getCPUTime)
import Control.Monad    (unless)

readGenericPackageDescription :: Verbosity -> FilePath
                              -> IO GenericPackageDescription
readGenericPackageDescription verbosity path =
  if (takeExtension path) == ".dhall" then
    readDhallGenericPackageDescription verbosity path
  else
    Cabal.Parse.readGenericPackageDescription verbosity path

readDhallGenericPackageDescription :: Verbosity -> FilePath
                                   -> IO GenericPackageDescription
readDhallGenericPackageDescription verbosity dhallFilePath = do
  exists <- doesFileExist dhallFilePath
  unless exists $
    die' verbosity $
      "Error Parsing: file \"" ++ dhallFilePath ++ "\" doesn't exist. Cannot continue."
  
  source <- StrictText.readFile dhallFilePath
  info verbosity $ "Reading package configuration from " ++ dhallFilePath
  start <- getCPUTime
  gpd <- explaining $ parseGenericPackageDescriptionFromDhall dhallFilePath source
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^(12 :: Integer))
  info verbosity $ "Configuration readed in " ++ show (diff :: Double) ++ " seconds"
  return gpd
  
  where explaining = if verbosity >= verbose then Dhall.detailed else id

parseCabalGenericPackageDescription :: String
                                    -> Maybe GenericPackageDescription
#ifdef CABAL_PARSEC
parseCabalGenericPackageDescription content =
        Cabal.Parse.parseGenericPackageDescriptionMaybe $ BS.Char8.pack content
#else
parseCabalGenericPackageDescription content =
      case Cabal.Parse.parseGenericPackageDescription content of
        ParseOk _ pkg -> Just pkg
        _             -> Nothing
#endif

parseGenericPackageDescriptionFromDhall :: FilePath -> StrictText.Text
                                        -> IO GenericPackageDescription  
parseGenericPackageDescriptionFromDhall dhallFilePath content = do
  let settings = Dhall.defaultInputSettings
         & Lens.set Dhall.rootDirectory ( takeDirectory dhallFilePath )
         & Lens.set Dhall.sourceName dhallFilePath
  fmap fixGPDConstraints $ dhallToCabal settings content


writeDerivedCabalFile :: Verbosity -> FilePath
                      -> GenericPackageDescription -> IO FilePath
writeDerivedCabalFile verbosity dir genPkg = do
  let path = dir </> "etlas.dhall.cabal"
  info verbosity $ "Writing derived cabal file from dhall file: " ++ path
  createDirectoryIfMissingVerbose verbosity True dir
  writeGenericPackageDescription path genPkg
  return path

-- TODO: Pick Lens modules from Cabal if we need them in more places
condLibrary' :: Lens' GenericPackageDescription (Maybe (CondTree ConfVar [Dependency] Library))
condLibrary' f s = fmap (\x -> s { condLibrary = x }) (f (condLibrary s))

condSubLibraries' :: Lens' GenericPackageDescription [(UnqualComponentName,(CondTree ConfVar [Dependency] Library))]
condSubLibraries' f s = fmap (\x -> s { condSubLibraries = x }) (f (condSubLibraries s))

condForeignLibs' :: Lens' GenericPackageDescription [(UnqualComponentName,(CondTree ConfVar [Dependency] ForeignLib))]
condForeignLibs' f s = fmap (\x -> s { condForeignLibs = x }) (f (condForeignLibs s))

condExecutables' :: Lens' GenericPackageDescription [(UnqualComponentName,(CondTree ConfVar [Dependency] Executable))]
condExecutables' f s = fmap (\x -> s { condExecutables = x }) (f (condExecutables s))

condTestSuites' :: Lens' GenericPackageDescription [(UnqualComponentName,(CondTree ConfVar [Dependency] TestSuite))]
condTestSuites' f s = fmap (\x -> s { condTestSuites = x }) (f (condTestSuites s))

condBenchmarks' :: Lens' GenericPackageDescription [(UnqualComponentName,(CondTree ConfVar [Dependency] Benchmark))]
condBenchmarks' f s = fmap (\x -> s { condBenchmarks = x }) (f (condBenchmarks s))

fixGPDConstraints
  :: GenericPackageDescription
  -> GenericPackageDescription
fixGPDConstraints
  = Lens.over ( condBenchmarks' . traverse . Lens._2 ) fixCondTreeConstraints
  . Lens.over ( condExecutables' . traverse . Lens._2 ) fixCondTreeConstraints
  . Lens.over ( condForeignLibs' . traverse . Lens._2 ) fixCondTreeConstraints
  . Lens.over ( condLibrary' . traverse ) fixCondTreeConstraints
  . Lens.over ( condSubLibraries' . traverse . Lens._2 ) fixCondTreeConstraints
  . Lens.over ( condTestSuites' . traverse . Lens._2 ) fixCondTreeConstraints

class HasBuildInfo a where
   buildInfo' :: Lens' a BuildInfo
   targetBuildDepends' :: Lens' a [Dependency]
   targetBuildDepends' = buildInfo' . targetBuildDepends'

instance HasBuildInfo BuildInfo where
   buildInfo' = id
   targetBuildDepends' f s = fmap (\x -> s { targetBuildDepends = x }) (f (targetBuildDepends s))

instance HasBuildInfo Benchmark where
    buildInfo' f (Benchmark x1 x2 x3) = fmap (\y1 -> Benchmark x1 x2 y1) (f x3)

instance HasBuildInfo Executable where
    buildInfo' f l = (\x -> l { buildInfo = x }) <$> f (buildInfo l)

instance HasBuildInfo ForeignLib where
    buildInfo' f l = (\x -> l { foreignLibBuildInfo = x }) <$> f (foreignLibBuildInfo l)

instance HasBuildInfo Library where
    buildInfo' f l = (\x -> l { libBuildInfo = x }) <$> f (libBuildInfo l)

instance HasBuildInfo TestSuite where
    buildInfo' f l = (\x -> l { testBuildInfo = x }) <$> f (testBuildInfo l)

fixCondTreeConstraints
  :: ( HasBuildInfo a )
  => CondTree v cs a
  -> CondTree v [Dependency] a
fixCondTreeConstraints ( CondNode a _ branches ) =
  CondNode a deps ( fixCondBranchConstraints <$> branches )
  where
  deps = Lens.view ( buildInfo' . targetBuildDepends' ) a

fixCondBranchConstraints
  :: ( HasBuildInfo a )
  => CondBranch v cs a
  -> CondBranch v [Dependency] a
fixCondBranchConstraints ( CondBranch cond true falseMay ) =
  CondBranch cond
    ( fixCondTreeConstraints true )
    ( fixCondTreeConstraints <$> falseMay )
