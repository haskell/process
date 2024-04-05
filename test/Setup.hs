{-# OPTIONS_GHC -Wall #-}

module Main (main) where

-- Cabal
import Distribution.Simple
  ( defaultMainWithHooks
  , simpleUserHooks
  , UserHooks(buildHook)
  )
import Distribution.Simple.BuildPaths
  ( autogenComponentModulesDir
  , exeExtension
  )
import Distribution.Simple.LocalBuildInfo
  ( hostPlatform
  , buildDir
  , withTestLBI
  )
import Distribution.Types.LocalBuildInfo
  ( LocalBuildInfo
  , allTargetsInBuildOrder'
  )
import Distribution.Types.Component
  ( Component(CExe) )
import Distribution.Types.Executable
  ( Executable(exeName) )
import Distribution.Types.PackageDescription
  ( PackageDescription )
import Distribution.Types.TargetInfo
  ( targetComponent )
import Distribution.Types.UnqualComponentName
  ( unUnqualComponentName )

-- directory
import System.Directory
  ( createDirectoryIfMissing )

-- filepath
import System.FilePath
  ( (</>), (<.>), takeDirectory )

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainWithHooks testProcessHooks

-- The following code works around Cabal bug #9854.
--
-- The process-tests package has an executable component named "cli-child",
-- used for testing. We want to invoke this executable when running tests;
-- however, due to the Cabal bug this executable does not get added to PATH.
-- To fix this, we create a "Test.Paths" module in a Custom setup script,
-- which contains paths to executables used for testing.
testProcessHooks :: UserHooks
testProcessHooks =
  simpleUserHooks
    { buildHook = \ pd lbi userHooks buildFlags ->
        withTestLBI pd lbi $ \ _testSuite clbi -> do
          let pathsFile = autogenComponentModulesDir lbi clbi </> "Test" </> "Paths" <.> "hs"
          createDirectoryIfMissing True (takeDirectory pathsFile)
          writeFile pathsFile $ unlines
            [ "module Test.Paths where"
            , "processInternalExes :: [(String, FilePath)]"
            , "processInternalExes = " ++ show (processInternalExes pd lbi)
            ]
          buildHook simpleUserHooks pd lbi userHooks buildFlags
    }

processInternalExes :: PackageDescription -> LocalBuildInfo -> [(String, FilePath)]
processInternalExes pd lbi =
  [ (toolName, toolLocation)
  | tgt <- allTargetsInBuildOrder' pd lbi
  , CExe exe <- [targetComponent tgt]
  , let toolName = unUnqualComponentName $ exeName exe
        toolLocation =
          buildDir lbi
            </> (toolName </> toolName <.> exeExtension (hostPlatform lbi))
  ]
