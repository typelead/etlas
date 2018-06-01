module Distribution.Client.Sandbox where

import Distribution.Client.Config        ( SavedConfig )
import Distribution.Client.GlobalFlags   ( GlobalFlags )
import Distribution.Client.Sandbox.Types ( UseSandbox )
import Distribution.Simple.Setup         ( Flag )
import Distribution.Verbosity            ( Verbosity )

findSavedDistPref :: SavedConfig -> Flag FilePath -> IO FilePath
loadConfigOrSandboxConfig :: Verbosity -> GlobalFlags -> IO (UseSandbox, SavedConfig)
