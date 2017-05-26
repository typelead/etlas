module Distribution.Client.Install where

import Distribution.Verbosity
import Distribution.System

import Distribution.Simple.Compiler
import Distribution.Simple.Program
import Distribution.Simple.Setup

import Distribution.Client.Setup as Setup
import Distribution.Client.Sandbox.Types
import Distribution.Client.Targets

install
  :: Verbosity
  -> PackageDBStack
  -> RepoContext
  -> Compiler
  -> Platform
  -> ProgramDb
  -> UseSandbox
  -> Maybe SandboxPackageInfo
  -> Setup.GlobalFlags
  -> ConfigFlags
  -> ConfigExFlags
  -> Setup.InstallFlags
  -> HaddockFlags
  -> [UserTarget]
  -> IO ()