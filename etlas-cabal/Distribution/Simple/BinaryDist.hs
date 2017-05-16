{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.BinaryDist
-- Copyright   :  Rahul Muttineni 2017
-- License     :  BSD3
--
-- Maintainer  :  rahulmutt@gmail.com
-- Portability :  portable
--
-- This handles the @bdist@ command. The module exports a 'bdist' action but
-- also some of the phases that make it up so that other tools can use just the
-- bits they need. In particular the preparation of the tree of files to go
-- into the source tarball is separated from actually building the source
-- tarball.
--
-- The 'createArchive' action uses the external @tar@ program and assumes that
-- it accepts the @-z@ flag. Neither of these assumptions are valid on Windows.
-- The 'bdist' action now also does some distribution QA checks.

-- NOTE: FIX: we don't have a great way of testing this module, since
-- we can't easily look inside a tarball once its created.

module Distribution.Simple.BinaryDist (
  -- * The top level action
  bdist,
  )  where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.PackageDescription hiding (Flag)
-- import Distribution.PackageDescription.Check hiding (doesFileExist)
import Distribution.Package
-- import Distribution.ModuleName
-- import qualified Distribution.ModuleName as ModuleName
-- import Distribution.Version
import Distribution.Simple.Eta
import Distribution.Simple.Utils
import Distribution.Simple.Install
import Distribution.Simple.Setup
-- import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo
-- import Distribution.Simple.BuildPaths
-- import Distribution.Simple.Program
-- import Distribution.Text
-- import Distribution.Types.ForeignLib
import Distribution.Verbosity

-- import Data.List (partition)
import qualified Data.Map as Map
-- import Data.Time (UTCTime, getCurrentTime, toGregorian, utctDay)
-- import System.Directory ( doesFileExist )
-- import System.IO (IOMode(WriteMode), hPutStrLn, withFile)
import System.FilePath ((</>), takeDirectory, takeFileName )
-- import Control.Monad

-- |Create a binary distribution.
bdist :: PackageDescription     -- ^information from the tarball
      -> LocalBuildInfo         -- ^Information from configure
      -> BDistFlags             -- ^verbosity & snapshot
      -> IO ()
bdist pkg lbi flags = do
  case flagToMaybe (bDistTargetDirectory flags) of
    Just targetDir -> do
      setupMessage verbosity "Building binary dist for" (packageId pkg)
      prepareTree verbosity pkg lbi targetDir
      info verbosity $ "Binary directory created: " ++ targetDir
    Nothing -> die' verbosity "Must supply a target directory."

  where verbosity = fromFlag (bDistVerbosity flags)
        -- distPref  = fromFlag $ bDistDistPref flags -- TODO: What to do?

-- |Prepare a directory tree of source files.
prepareTree :: Verbosity          -- ^verbosity
            -> PackageDescription -- ^info from the cabal file
            -> LocalBuildInfo
            -> FilePath           -- ^source tree to populate
            -> IO ()
prepareTree verbosity pkg_descr lbi targetDir = do
  withCLBI lbi $ \clbi -> do
    lib <- getLibrary pkg_descr
    let builtDir = componentBuildDir lbi clbi
        targetBuiltDir = targetDir </> builtDir
    -- Copy install-include files
    installIncludeFiles verbosity lib targetDir
    sequence_
      [ do createDirectoryIfMissingVerbose verbosity True $
             targetDir </> takeDirectory lfile
           installOrdinaryFile verbosity lfile (targetDir </> lfile)
      | lfile <- licenseFiles pkg_descr ]
    -- Copy .hi files
    findModuleFiles [builtDir] ["hi"] (allLibModules lib clbi)
      >>= installOrdinaryFiles verbosity targetBuiltDir
    -- Copy lib jar
    let jarLibName = mkJarName (componentUnitId clbi)
    installOrdinaryFile verbosity (builtDir </> jarLibName)
                                  (targetBuiltDir </> jarLibName)
    -- Copy cabal file
    cabalFile <- defaultPackageDesc verbosity
    installOrdinaryFile verbosity cabalFile (targetDir </> takeFileName cabalFile)

  where withCLBI lbi' action =
          case Map.lookup CLibName (componentNameMap lbi') of
            Just [clbi] -> action clbi
            _ -> die' verbosity "Cannot find library component info."
        getLibrary pkgDescr =
          case library pkgDescr of
            Just lib -> return lib
            Nothing -> die' verbosity "Cannot build binary distributions for executables."
