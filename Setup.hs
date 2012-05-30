#!/usr/bin/env runhaskell
{-# LANGUAGE BangPatterns #-}

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Data.Char
import System.Exit
import System.IO
import System.Directory
import System.FilePath.Windows

main = let hooks = simpleUserHooks 
       in defaultMainWithHooks hooks { confHook  = hailsConfigure
                                     , cleanHook = hailsClean
                                     }

-- This is a non-portable version of hails. It generates the
-- files inside the src dir.
hailsConfigure :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
hailsConfigure (gpd, info) flags = do
  runProgramInvocation verbosity $ simpleProgramInvocation "hails" ["--init", "--output-dir=src"]
  (confHook simpleUserHooks) (gpd, info) flags 
 where verbosity = fromFlag $ configVerbosity flags

hailsClean :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
hailsClean pd () hooks flags = do
  runProgramInvocation verbosity $ simpleProgramInvocation "hails" ["--clean", "--output-dir=src"]
  (cleanHook simpleUserHooks) pd () hooks flags
 where verbosity = fromFlag $ cleanVerbosity flags
