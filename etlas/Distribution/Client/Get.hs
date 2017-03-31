{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Get
-- Copyright   :  (c) Andrea Vezzosi 2008
--                    Duncan Coutts 2011
--                    John Millikin 2012
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'cabal get' command.
-----------------------------------------------------------------------------

module Distribution.Client.Get (
    get
  ) where

import Distribution.Package
         ( PackageId, packageId, packageName )
import Distribution.Simple.Setup
         ( Flag(..), fromFlag, fromFlagOrDefault )
import Distribution.Simple.Utils
         ( notice, die, info, writeFileAtomic )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text(display)
import qualified Distribution.PackageDescription as PD

import Distribution.Client.Setup
         ( GlobalFlags(..), GetFlags(..) )
import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.Patch
import Distribution.Client.Brancher
import Distribution.Client.FetchUtils
import qualified Distribution.Client.Tar as Tar (extractTarGzFile)
import Distribution.Client.IndexUtils as IndexUtils
        ( getSourcePackages )
import Distribution.Client.Compat.Process
        ( readProcessWithExitCode )
import Distribution.Compat.Exception
        ( catchIO )
import Distribution.Client.Config  ( defaultPatchesDir )
import Control.Exception
         ( finally )
import Control.Monad
         ( filterM, forM_, unless, when )
import Data.List
         ( sortBy )
import qualified Data.Map
import Data.Maybe
         ( listToMaybe, mapMaybe )
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
         ( mempty )
#endif
import Data.Ord
         ( comparing )
import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
         , getCurrentDirectory, setCurrentDirectory
         )
import System.Exit
         ( ExitCode(..) )
import System.FilePath
         ( (</>), (<.>), addTrailingPathSeparator )
import System.Process
         ( rawSystem )


-- | Entry point for the 'cabal get' command.
get :: Verbosity
    -> [Repo]
    -> GlobalFlags
    -> GetFlags
    -> [UserTarget]
    -> IO ()
get verbosity _ _ _ [] =
    notice verbosity "No packages requested. Nothing to do."

get verbosity repos globalFlags getFlags userTargets = do
  let useFork = case (getSourceRepository getFlags) of
        NoFlag -> False
        _      -> True

  unless useFork $
    mapM_ checkTarget userTargets

  sourcePkgDb <- getSourcePackages verbosity repos

  pkgSpecifiers <- resolveUserTargets verbosity
                   (fromFlag $ globalWorldFile globalFlags)
                   (packageIndex sourcePkgDb)
                   userTargets

  pkgs <- either (die . unlines . map show) return $
            resolveWithoutDependencies
              (resolverParams sourcePkgDb pkgSpecifiers)

  unless (null prefix) $
    createDirectoryIfMissing True prefix

  if useFork
    then fork pkgs
    else unpack pkgs

  where
    resolverParams sourcePkgDb pkgSpecifiers =
        --TODO: add command-line constraint and preference args for unpack
        standardInstallPolicy mempty sourcePkgDb pkgSpecifiers

    prefix = fromFlagOrDefault "" (getDestDir getFlags)

    fork :: [SourcePackage] -> IO ()
    fork pkgs = do
      let kind = fromFlag . getSourceRepository $ getFlags
      branchers <- findUsableBranchers
      mapM_ (\pkg -> forkPackage verbosity branchers
                       (prefix </> (display (packageName pkg)))
                       kind (packageId pkg) (PD.sourceRepos $
                                             PD.packageDescription $
                                             packageDescription pkg)) pkgs

    unpack :: [SourcePackage] -> IO ()
    unpack pkgs = do
      forM_ pkgs $ \pkg -> do
        location <- fetchPackage verbosity (packageSource pkg)
        let pkgid = packageId pkg
            descOverride | usePristine = Nothing
                         | otherwise   = packageDescrOverride pkg
        case location of
          LocalTarballPackage tarballPath ->
            unpackPackage verbosity prefix pkgid descOverride tarballPath False

          RemoteTarballPackage _tarballURL tarballPath ->
            unpackPackage verbosity prefix pkgid descOverride tarballPath False

          RepoTarballPackage _repo _pkgid tarballPath ->
            unpackPackage verbosity prefix pkgid descOverride tarballPath False

          ScmPackage _ _ _ localDirPath ->
            unpackPackage verbosity prefix pkgid descOverride localDirPath True

          LocalUnpackedPackage _ ->
            error "Distribution.Client.Get.unpack: the impossible happened."
      where
        usePristine = fromFlagOrDefault False (getPristine getFlags)

checkTarget :: UserTarget -> IO ()
checkTarget target = case target of
    UserTargetLocalDir       dir  -> die (notTarball dir)
    UserTargetLocalCabalFile file -> die (notTarball file)
    _                             -> return ()
  where
    notTarball t =
        "The 'get' command is for tarball packages. "
     ++ "The target '" ++ t ++ "' is not a tarball."

-- ------------------------------------------------------------
-- * Unpacking the source tarball
-- ------------------------------------------------------------

unpackPackage :: Verbosity -> FilePath -> PackageId
              -> PackageDescriptionOverride
              -> FilePath -> Bool -> IO ()
unpackPackage verbosity prefix pkgid descOverride pkgPath isGit = do
    let pkgdirname = display pkgid
        pkgdir     = prefix </> pkgdirname
        pkgdir'    = addTrailingPathSeparator pkgdir
    existsDir  <- doesDirectoryExist pkgdir
    when existsDir $ die $
     "The directory \"" ++ pkgdir' ++ "\" already exists, not unpacking."
    existsFile  <- doesFileExist pkgdir
    when existsFile $ die $
     "A file \"" ++ pkgdir ++ "\" is in the way, not unpacking."
    notice verbosity $ "Unpacking to " ++ pkgdir'
    patchedExtractTarGzFile verbosity True prefix pkgdirname pkgPath defaultPatchesDir isGit

    case descOverride of
      Nothing     -> return ()
      Just pkgtxt -> do
        let descFilePath = pkgdir </> display (packageName pkgid) <.> "cabal"
        info verbosity $
          "Updating " ++ descFilePath
                      ++ " with the latest revision from the index."
        writeFileAtomic descFilePath pkgtxt
