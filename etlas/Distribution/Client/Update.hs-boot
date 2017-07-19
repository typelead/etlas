module Distribution.Client.Update where

import Distribution.Verbosity (Verbosity)
import Distribution.Client.Setup (UpdateFlags)
import Distribution.Client.GlobalFlags (RepoContext)

update :: Verbosity -> UpdateFlags -> RepoContext -> IO ()