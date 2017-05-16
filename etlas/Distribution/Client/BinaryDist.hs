{-# LANGUAGE CPP #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleContexts #-}
-- Implements the \"@.\/etlas bdist@\" command, which creates a binary
-- distribution for this package.  That is, packs up the compiled binaries
-- into a tarball, making use of the corresponding Cabal module.
module Distribution.Client.BinaryDist (
         bdist
  )  where


import Distribution.Client.SetupWrapper
        ( SetupScriptOptions(..), defaultSetupScriptOptions, setupWrapper )
import Distribution.Client.SrcDist

import Distribution.Package
         ( Package(..), packageName )
import Distribution.PackageDescription
         ( PackageDescription )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
#ifdef CABAL_PARSEC
import Distribution.PackageDescription.Parsec
         ( readGenericPackageDescription )
#else
import Distribution.PackageDescription.Parse
         ( readGenericPackageDescription )
#endif
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, defaultPackageDesc
         , warn, die', notice, withTempDirectory )
import Distribution.Client.Setup
         ( BDistFlags(..), BDistExFlags(..), ArchiveFormat(..) )
import Distribution.Simple.Setup
         ( Flag(..), bdistCommand, flagToList, fromFlag, fromFlagOrDefault
         , defaultBDistFlags )
import Distribution.Simple.BuildPaths ( binPref )
import Distribution.Simple.Program (requireProgram, simpleProgram, programPath)
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.Text ( display )
import Distribution.Verbosity (Verbosity, normal, lessVerbose)
import Distribution.Version   (mkVersion, orLaterVersion, intersectVersionRanges)

import Distribution.Client.Utils
  (tryFindAddSourcePackageDesc)
import Distribution.Compat.Exception                 (catchIO)

import System.FilePath ((</>), (<.>))
import Control.Monad (when, unless, liftM)
import System.Directory (doesFileExist, removeFile, canonicalizePath, getTemporaryDirectory)
import System.Process (runProcess, waitForProcess)
import System.Exit    (ExitCode(..))
import Control.Exception                             (IOException, evaluate)

-- |Create a binary distribution.
bdist :: BDistFlags -> BDistExFlags -> IO ()
bdist flags exflags = do
  pkg <- liftM flattenPackageDescription
    (readGenericPackageDescription verbosity =<< defaultPackageDesc verbosity)
  let withDir :: (FilePath -> IO a) -> IO a
      withDir = withTempDirectory verbosity tmpTargetDir "sdist."
  -- TODO: Transfer all the copied files to dist/bin if the user requests it
  -- createDirectoryIfMissingVerbose verbosity True tmpTargetDir
  withDir $ \tmpDir -> do
    let outDir = tmpDir </> tarBallName pkg
        flags' = flags { bDistTargetDirectory = Flag outDir }

    createDirectoryIfMissingVerbose verbosity True outDir

    setupWrapper verbosity setupOpts (Just pkg) bdistCommand (const flags') []

    createArchive verbosity pkg tmpDir distPref

  where
    flagEnabled f  = not . null . flagToList . f $ flags
    verbosity       = fromFlag (bDistVerbosity flags)
    distPref        = fromFlag (bDistDistPref flags)
    tmpTargetDir    = binPref distPref
    setupOpts       = defaultSetupScriptOptions {
      useDistPref     = distPref
    }
    format        = fromFlag (bDistFormat exflags)
    createArchive = case format of
      TargzFormat -> createTarGzArchive True
      ZipFormat   -> createZipArchive True

tarBallName :: PackageDescription -> String
tarBallName = (++ "-bin") . display . packageId
