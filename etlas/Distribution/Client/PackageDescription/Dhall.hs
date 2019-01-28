{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings #-}
module Distribution.Client.PackageDescription.Dhall where

import Control.Exception ( throwIO )
import qualified Control.Monad.Trans.State.Strict as State

import qualified Crypto.Hash

import Data.Function ( (&) )
import qualified Data.Hashable as Hashable
import Data.Maybe ( fromMaybe )
import Data.Semigroup ( (<>) )

import qualified Data.Text as StrictText
import qualified Data.Text.IO as StrictText

import Data.Word ( Word64 )

import qualified Dhall
import qualified Dhall.Binary as Dhall
import qualified Dhall.Core as Dhall
  hiding ( Type )
import qualified Dhall.Context
import qualified Dhall.Import as Dhall
  hiding ( startingContext, standardVersion )
import qualified Dhall.Import ( standardVersion )
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall

import DhallToCabal ( dhallToCabal, genericPackageDescription  ) 

import Distribution.Verbosity
import Distribution.PackageDescription.PrettyPrint
       ( writeGenericPackageDescription )
#ifdef CABAL_PARSEC
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Distribution.PackageDescription.Parsec as Cabal.Parse
       ( readGenericPackageDescription
       , parseGenericPackageDescriptionMaybe
       ) 
#else
import Distribution.PackageDescription.Parse as Cabal.Parse
       ( readGenericPackageDescription
       , parseGenericPackageDescription
       , ParseResult(..)
       )
#endif
import Distribution.Simple.Utils
       ( info, createDirectoryIfMissingVerbose )
import Distribution.PackageDescription
import Distribution.Types.Dependency
import Distribution.Types.ForeignLib
import Distribution.Types.UnqualComponentName
import Distribution.Types.CondTree

import qualified Lens.Micro as Lens
import Lens.Micro ( Lens' )
import qualified Lens.Micro.Extras as Lens

import Numeric (showHex)

import System.CPUTime ( getCPUTime )
import System.Directory
       ( createDirectoryIfMissing
       , doesFileExist
       , canonicalizePath
       )
import System.FilePath
       ( takeDirectory
       , takeExtension
       , (</>) )
                
readGenericPackageDescription :: Verbosity -> FilePath
                              -> IO GenericPackageDescription
readGenericPackageDescription verbosity path =
  if (takeExtension path) == ".dhall" then
    readCachedDhallGenericPackageDescription verbosity path
  else
    Cabal.Parse.readGenericPackageDescription verbosity path

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

measuringTime :: Verbosity -> String -> IO a -> IO a
measuringTime verbosity msg action = do
  start <- getCPUTime
  x <- action
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^(12 :: Integer))
  info verbosity $ msg ++ show (diff :: Double) ++ " seconds"
  return x
  
readCachedDhallGenericPackageDescription :: Verbosity -> FilePath
                                         -> IO GenericPackageDescription
readCachedDhallGenericPackageDescription verbosity dhallFilePath  = do

  fileWithDhallHashPath <- getFileWithDhallHashFilePath dhallFilePath
  exists <- doesFileExist fileWithDhallHashPath

  gpd <-
    if exists then measuringTime verbosity "Configuration readed in " $ do
      
      expectedHash <- StrictText.readFile fileWithDhallHashPath
      let expectedHashStr = StrictText.unpack expectedHash

      info verbosity $ "Reading package configuration from dhall cache using hash: "
        ++ expectedHashStr ++ " stored in file: " ++ fileWithDhallHashPath
      
      let cacheImport = "missing sha256:" <> expectedHash 
      parseGenericPackageDescriptionFromDhall dhallFilePath cacheImport

    else do
      info verbosity $ "Missing file with dhall cache hash: "
                     ++ fileWithDhallHashPath
      readAndCacheGenericPackageDescriptionFromDhall verbosity dhallFilePath

  let explaining = if verbosity >= verbose then Dhall.detailed else id

  explaining $ return gpd
  
parseGenericPackageDescriptionFromDhall :: FilePath -> StrictText.Text
                                        -> IO GenericPackageDescription  
parseGenericPackageDescriptionFromDhall dhallFilePath src = do
  let settings = Dhall.defaultInputSettings
         & Lens.set Dhall.rootDirectory ( takeDirectory dhallFilePath )
         & Lens.set Dhall.sourceName dhallFilePath
  fmap fixGPDConstraints $ dhallToCabal settings src

readAndCacheGenericPackageDescriptionFromDhall :: Verbosity
                                               -> FilePath
                                               -> IO GenericPackageDescription
readAndCacheGenericPackageDescriptionFromDhall verbosity dhallFilePath = do
  info verbosity $ "Reading and caching package configuration from dhall file: "
                     ++ dhallFilePath
  measuringTime verbosity "Configuration readed in " $ do
    src <- StrictText.readFile dhallFilePath
    parseAndCacheGenericPackageDescriptionFromDhall dhallFilePath src

parseAndCacheGenericPackageDescriptionFromDhall :: FilePath -> StrictText.Text
                                                -> IO GenericPackageDescription 
parseAndCacheGenericPackageDescriptionFromDhall dhallFilePath src = do
  let Dhall.Type {..} = genericPackageDescription
  ( hash, normExpr ) <- parseAndHashGenericPackageDescriptionFromDhall
                          dhallFilePath src
  cacheAndExtractGenericPackageDescriptionFromDhall hash normExpr dhallFilePath

parseAndHashGenericPackageDescriptionFromDhall :: FilePath -> StrictText.Text
                                                -> IO ( Crypto.Hash.Digest Crypto.Hash.SHA256
                                                      , Dhall.Expr Dhall.Src Dhall.X
                                                      ) 
parseAndHashGenericPackageDescriptionFromDhall dhallFilePath src = do
  let  settings = Dhall.defaultInputSettings
         & Lens.set Dhall.rootDirectory ( takeDirectory dhallFilePath )
         & Lens.set Dhall.sourceName dhallFilePath
  expr  <- Dhall.inputExprWithSettings settings src
  let normExpr = Dhall.alphaNormalize expr
      hash = Dhall.hashExpression Dhall.defaultStandardVersion normExpr
  return ( hash, normExpr )


cacheAndExtractGenericPackageDescriptionFromDhall :: Crypto.Hash.Digest Crypto.Hash.SHA256
                                                  -> Dhall.Expr Dhall.Src Dhall.X
                                                  -> FilePath
                                                  -> IO GenericPackageDescription
cacheAndExtractGenericPackageDescriptionFromDhall hash normExpr dhallFilePath = do
  gpd <- extractGenericPackageDescriptionFromDhall normExpr
  writeDhallToCache hash normExpr
  writeFileWithDhallHash hash dhallFilePath 
  return gpd

extractGenericPackageDescriptionFromDhall :: Dhall.Expr Dhall.Src Dhall.X
                                          -> IO GenericPackageDescription
extractGenericPackageDescriptionFromDhall expr = do                                        
  let Dhall.Type {..} = genericPackageDescription
      annot = ( ( Dhall.Annot expr expected )
                :: Dhall.Expr Dhall.Src Dhall.X )
  _ <- throws ( Dhall.typeWith Dhall.Context.empty annot )
  return $ fixGPDConstraints ( fromMaybe
                               ( error "Empty extracted GenericPackageDescription" )
                               ( extract expr ) )
  where throws = either Control.Exception.throwIO return


writeDhallToCache :: Crypto.Hash.Digest Crypto.Hash.SHA256
                  -> Dhall.Expr Dhall.Src Dhall.X
                  -> IO ()
writeDhallToCache hash expr  = do
  let status = Lens.set Dhall.Import.standardVersion
                 Dhall.defaultStandardVersion (Dhall.emptyStatus ".")
      newImportHashed =
        Dhall.ImportHashed { Dhall.hash = Just hash
                           , Dhall.importType = Dhall.Missing
                           }
      newImport =
        Dhall.Import { Dhall.importHashed = newImportHashed
                     , Dhall.importMode = Dhall.Code
                     }
  State.evalStateT (Dhall.exprToImport newImport expr) status

writeFileWithDhallHash :: Crypto.Hash.Digest Crypto.Hash.SHA256
                       -> FilePath -> IO ()
writeFileWithDhallHash hash dhallFilePath = do
  path <- getFileWithDhallHashFilePath dhallFilePath
  createDirectoryIfMissing True $ takeDirectory path
  StrictText.writeFile path ( StrictText.pack ( show hash ) )  

getFileWithDhallHashFilePath :: FilePath -> IO FilePath 
getFileWithDhallHashFilePath dhallFilePath = do
  let cacheDir = takeDirectory dhallFilePath </> "dist" </> "cache"
  hashFileName <- getFileWithDhallHashFileName dhallFilePath
  return $ cacheDir </> hashFileName

getFileWithDhallHashFileName :: FilePath -> IO FilePath
getFileWithDhallHashFileName dhallFilePath = do
  canonPath <- canonicalizePath dhallFilePath
  let hash = Hashable.hash canonPath
  return $  showHex ( ( fromIntegral hash ) :: Word64 ) ""
          
writeDerivedCabalFile :: Verbosity -> FilePath
                      -> GenericPackageDescription -> IO ()
writeDerivedCabalFile verbosity path genPkg = do
  info verbosity $ "Writing derived cabal file from dhall file: " ++ path
  let dir = takeDirectory path
  createDirectoryIfMissingVerbose verbosity True dir
  writeGenericPackageDescription path genPkg

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
