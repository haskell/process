{-# LANGUAGE CPP #-}
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
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path
  ( SymbolicPathX, interpretSymbolicPathCWD)
#endif

-- directory
import System.Directory
  ( createDirectoryIfMissing )

-- filepath
import System.FilePath
  ( (</>), (<.>), takeDirectory )

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainWithHooks testProcessHooks

#if MIN_VERSION_Cabal(3,14,0)
pathToString ::
  SymbolicPathX allowAbsolute from to -> String
pathToString = interpretSymbolicPathCWD
#else
pathToString :: String -> String
pathToString = id
#endif

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
          let
              baseDir = pathToString (autogenComponentModulesDir lbi clbi) </> "Test"
              exes = processInternalExes pd lbi

          -- Test.Paths.String
          do
            let
                pathsFile = baseDir </> "String" </> "Paths" <.> "hs"
                wrap string = show string
            createDirectoryIfMissing True (takeDirectory pathsFile)
            writeFile pathsFile $ unlines
              [ "module Test.String.Paths where"
              , "processInternalExes :: [(String, FilePath)]"
              , "processInternalExes = " ++ printExes wrap exes
              ]

          -- Test.Paths.OsString
          do
            let
                pathsFile = baseDir </> "OsString" </> "Paths" <.> "hs"
                wrap string = "[osp|" ++ string ++ "|]"
            createDirectoryIfMissing True (takeDirectory pathsFile)
            writeFile pathsFile $ unlines
              [ "{-# LANGUAGE QuasiQuotes #-}"
              , "module Test.OsString.Paths where"
              , "import System.OsPath (OsPath, osp)"
              , "processInternalExes :: [(String, OsPath)]"
              , "processInternalExes = " ++ printExes wrap exes
              ]

          buildHook simpleUserHooks pd lbi userHooks buildFlags
    }

printExes :: (String -> String) -> [(String, FilePath)] -> String
printExes wrap list = "[" ++ drop 2 (concat list') ++ "]"
    where
        list' :: [String]
        list' =
            [ ", (" ++ show toolName ++ ", " ++ wrap toolLocation ++ ")"
            | (toolName, toolLocation) <- list
            ]

processInternalExes :: PackageDescription -> LocalBuildInfo -> [(String, FilePath)]
processInternalExes pd lbi =
  [ (toolName, toolLocation)
  | tgt <- allTargetsInBuildOrder' pd lbi
  , CExe exe <- [targetComponent tgt]
  , let toolName = unUnqualComponentName $ exeName exe
        toolLocation =
          pathToString (buildDir lbi)
            </> (toolName </> toolName <.> exeExtension (hostPlatform lbi))
  ]
