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

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (get)

import Distribution.Package
         ( PackageId, packageId, packageName )
import Distribution.Simple.Setup
         ( Flag(..), fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.Utils
         ( notice, die' )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text(display)
import qualified Distribution.PackageDescription as PD

import Distribution.Client.Setup
         ( GlobalFlags(..), GetFlags(..), RepoContext(..) )
import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Brancher
import Distribution.Client.Dependency
import Distribution.Client.FetchUtils
import Distribution.Client.Patch
import Distribution.Client.IndexUtils as IndexUtils
        ( getSourcePackagesAtIndexState )

import Distribution.Solver.Types.SourcePackage

import Control.Monad
         ( forM_, mapM_ )
import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist )
import System.FilePath
         ( (</>), addTrailingPathSeparator )


-- | Entry point for the 'cabal get' command.
get :: Verbosity
    -> RepoContext
    -> GlobalFlags
    -> GetFlags
    -> [UserTarget]
    -> IO ()
get verbosity _ _ _ [] =
    notice verbosity "No packages requested. Nothing to do."

get verbosity repoCtxt globalFlags getFlags userTargets = do
  let useFork = case (getSourceRepository getFlags) of
        NoFlag -> False
        _      -> True

  unless useFork $
    mapM_ (checkTarget verbosity) userTargets

  let idxState = flagToMaybe $ getIndexState getFlags

  sourcePkgDb <- getSourcePackagesAtIndexState verbosity repoCtxt binariesPath idxState

  pkgSpecifiers <- resolveUserTargets verbosity repoCtxt
                   (fromFlag $ globalWorldFile globalFlags)
                   (packageIndex sourcePkgDb)
                   userTargets

  pkgs <- either (die' verbosity . unlines . map show) return $
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

    binariesPath = fromFlag $ globalBinariesDir globalFlags

    prefix = fromFlagOrDefault "" (getDestDir getFlags)


    fork :: [UnresolvedSourcePackage] -> IO ()
    fork pkgs = do
      let kind = fromFlag . getSourceRepository $ getFlags
      branchers <- findUsableBranchers
      mapM_ (\pkg -> forkPackage verbosity branchers
                       (prefix </> (display (packageName pkg)))
                       kind (Right (packageId pkg))
                       (PD.sourceRepos $ PD.packageDescription $
                        packageDescription pkg)) pkgs

    unpack :: [UnresolvedSourcePackage] -> IO ()
    unpack pkgs = do
      forM_ pkgs $ \pkg -> do
        location <- fetchPackage verbosity repoCtxt (packageSource pkg)
        let pkgid = packageId pkg
            descOverride | usePristine = Nothing
                         | otherwise   = packageDescrOverride pkg

        case location of
          LocalTarballPackage tarballPath _isBinary ->
            unpackPackage verbosity prefix pkgid descOverride tarballPath False patchesDir

          RemoteTarballPackage _tarballURL tarballPath ->
            unpackPackage verbosity prefix pkgid descOverride tarballPath False patchesDir

          RepoTarballPackage _repo _pkgid tarballPath ->
            unpackPackage verbosity prefix pkgid descOverride tarballPath False patchesDir

          ScmPackage _ _ _ localDirPath ->
            unpackPackage verbosity prefix pkgid descOverride localDirPath True patchesDir

          LocalUnpackedPackage _ ->
            error "Distribution.Client.Get.unpack: the impossible happened."
      where
        usePristine = fromFlagOrDefault False (getPristine getFlags)
        patchesDir  = fromFlag (globalPatchesDir globalFlags)

checkTarget :: Verbosity -> UserTarget -> IO ()
checkTarget verbosity target = case target of
    UserTargetLocalDir       dir  -> die' verbosity (notTarball dir)
    UserTargetLocalCabalFile file -> die' verbosity (notTarball file)
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
              -> FilePath -> Bool -> FilePath -> IO ()
unpackPackage verbosity prefix pkgid _descOverride pkgPath isGit patchesDir = do
    let pkgdirname = display pkgid
        pkgdir     = prefix </> pkgdirname
        pkgdir'    = addTrailingPathSeparator pkgdir
    existsDir  <- doesDirectoryExist pkgdir
    when existsDir $ die' verbosity $
     "The directory \"" ++ pkgdir' ++ "\" already exists, not unpacking."
    existsFile  <- doesFileExist pkgdir
    when existsFile $ die' verbosity $
     "A file \"" ++ pkgdir ++ "\" is in the way, not unpacking."
    notice verbosity $ "Unpacking to " ++ pkgdir'
    patchedExtractTarGzFile verbosity True prefix pkgdirname pkgPath patchesDir isGit False

    -- case descOverride of
    --   Nothing     -> return ()
    --   Just pkgtxt -> do
    --     let descFilePath = pkgdir </> display (packageName pkgid) <.> "cabal"
    --     info verbosity $
    --       "Updating " ++ descFilePath
    --                   ++ " with the latest revision from the index."
    --     writeFileAtomic descFilePath pkgtxt
