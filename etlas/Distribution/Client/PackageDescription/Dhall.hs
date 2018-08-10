module Distribution.Client.PackageDescription.Dhall where

import Data.Function ( (&) )
import qualified Data.Text.IO as StrictText

import qualified Dhall
import DhallToCabal (dhallToCabal)

import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.Simple.Utils (die')

import Lens.Micro ( set )

import System.Directory (doesFileExist)
import System.FilePath (takeDirectory)

import Control.Monad    (unless)

readGenericPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription verbosity dhallFilePath = do
  exists <- doesFileExist dhallFilePath
  unless exists $
    die' verbosity $
      "Error Parsing: file \"" ++ dhallFilePath ++ "\" doesn't exist. Cannot continue."
  
  let settings = Dhall.defaultInputSettings
        & set Dhall.rootDirectory ( takeDirectory dhallFilePath )
        & set Dhall.sourceName dhallFilePath

  source <- StrictText.readFile dhallFilePath

  explaining $ dhallToCabal settings source

  where
    explaining = if verbosity >= verbose then Dhall.detailed else id
