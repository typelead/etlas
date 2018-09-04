{-# LANGUAGE CPP #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleContexts #-}
-- Implements the \"@.\/etlas bdist@\" command, which creates a binary
-- distribution for this package.  That is, packs up the compiled binaries
-- into a tarball, making use of the corresponding Cabal module.
module Distribution.Client.BinaryDist (
         bdist
       , tarBallName
  )  where


import Distribution.Client.SetupWrapper
        ( SetupScriptOptions(..), defaultSetupScriptOptions, setupWrapper )
import Distribution.Client.SrcDist

import Distribution.Package
         ( Package(..) )
import Distribution.PackageDescription
         ( PackageDescription )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.Client.PackageDescription.Dhall
         ( readGenericPackageDescription )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, defaultPackageDesc
         , withTempDirectory )
import Distribution.Client.Setup
         ( BDistFlags(..), BDistExFlags(..), ArchiveFormat(..) )
import Distribution.Simple.Setup
         ( Flag(..), bdistCommand, fromFlag, fromFlagOrDefault )
import Distribution.Simple.BuildPaths ( binPref )
import Distribution.Text ( display )

import System.FilePath ((</>))

-- |Create a binary distribution.
bdist :: BDistFlags -> BDistExFlags -> IO ()
bdist flags exflags = do
  genPkg <- readGenericPackageDescription verbosity =<< defaultPackageDesc verbosity
  let pkg = flattenPackageDescription genPkg

  let withDir :: (FilePath -> IO a) -> IO a
      withDir = withTempDirectory verbosity tmpTargetDir "bdist."

  -- TODO: Transfer all the copied files to dist/bin if the user requests it
  createDirectoryIfMissingVerbose verbosity True tmpTargetDir
  withDir $ \tmpDir -> do
    let outDir = tmpDir </> tarBallName pkg
        flags' = flags { bDistTargetDirectory = Flag outDir }

    createDirectoryIfMissingVerbose verbosity True outDir
    createDirectoryIfMissingVerbose verbosity True tarBallPath

    setupWrapper verbosity setupOpts (Just genPkg) (Just pkg) bdistCommand (const flags') []

    createArchive verbosity pkg tmpDir tarBallPath

  where
    verbosity       = fromFlag (bDistVerbosity flags)
    distPref        = fromFlag (bDistDistPref flags)
    tarBallPath     = fromFlagOrDefault distPref (bDistTargetDirectory flags)
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
