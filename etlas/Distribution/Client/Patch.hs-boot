module Distribution.Client.Patch where

import Distribution.Package        ( PackageIdentifier )
import System.FilePath ( FilePath )
import qualified Data.ByteString.Lazy as BS

patchedTarPackageCabalFile :: FilePath
                           -> IO FilePath
                           -> IO (Maybe (FilePath, BS.ByteString))
patchedPackageCabalFile :: PackageIdentifier
                        -> IO FilePath
                        -> IO (Maybe BS.ByteString)
