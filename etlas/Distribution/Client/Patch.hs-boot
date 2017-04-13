module Distribution.Client.Patch where

import Distribution.Package ( PackageIdentifier )
import System.FilePath      ( FilePath )
import qualified Data.ByteString.Lazy as BS

patchedTarPackageCabalFile :: FilePath
                           -> FilePath
                           -> IO (Maybe (FilePath, BS.ByteString))
patchedPackageCabalFile :: PackageIdentifier
                        -> FilePath
                        -> IO (Maybe BS.ByteString)
