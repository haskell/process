
module Main (main) where

import Data.List
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Setup
import Distribution.Simple.LocalBuildInfo
import System.Environment

main :: IO ()
main = do args <- getArgs
          let (ghcArgs, args') = extractGhcArgs args
              (confArgs, args'') = extractConfigureArgs args'
              hooks = defaultUserHooks {
                  postConf = add_configure_options confArgs
                           $ postConf defaultUserHooks,
                  buildHook = add_ghc_options ghcArgs
                            $ filter_modules_hook
                            $ buildHook defaultUserHooks,
                  makefileHook = add_ghc_options ghcArgs
                               $ filter_modules_hook
                               $ makefileHook defaultUserHooks,
                  instHook = filter_modules_hook
                           $ instHook defaultUserHooks }
          withArgs args'' $ defaultMainWithHooks hooks

extractGhcArgs :: [String] -> ([String], [String])
extractGhcArgs = extractPrefixArgs "--ghc-option="

extractConfigureArgs :: [String] -> ([String], [String])
extractConfigureArgs = extractPrefixArgs "--configure-option="

extractPrefixArgs :: String -> [String] -> ([String], [String])
extractPrefixArgs the_prefix args
 = let f [] = ([], [])
       f (x:xs) = case f xs of
                      (wantedArgs, otherArgs) ->
                          case removePrefix the_prefix x of
                              Just wantedArg ->
                                  (wantedArg:wantedArgs, otherArgs)
                              Nothing ->
                                  (wantedArgs, x:otherArgs)
   in f args

removePrefix :: String -> String -> Maybe String
removePrefix "" ys = Just ys
removePrefix _  "" = Nothing
removePrefix (x:xs) (y:ys)
 | x == y = removePrefix xs ys
 | otherwise = Nothing

type PostConfHook = Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo
                 -> IO ()

add_configure_options :: [String] -> PostConfHook -> PostConfHook
add_configure_options args f as cfs pd lbi
 = f (as ++ args) cfs pd lbi

type Hook a = PackageDescription -> LocalBuildInfo -> UserHooks -> a -> IO ()

add_ghc_options :: [String] -> Hook a -> Hook a
add_ghc_options args f pd lbi uhs x
 = do let lib' = case library pd of
                     Just lib ->
                         let bi = libBuildInfo lib
                             opts = options bi ++ [(GHC, args)]
                             bi' = bi { options = opts }
                         in lib { libBuildInfo = bi' }
                     Nothing -> error "Expected a library"
          pd' = pd { library = Just lib' }
      f pd' lbi uhs x

filter_modules_hook :: Hook a -> Hook a
filter_modules_hook f pd lbi uhs x
 = let build_filter = case compilerFlavor $ compiler lbi of
                          GHC -> const True
                          _ -> isPortableBuild
       lib' = case library pd of
                  Just lib ->
                      let ems = filter build_filter (exposedModules lib)
                      in lib { exposedModules = ems }
                  Nothing -> error "Expected a library"
       pd' = pd { library = Just lib' }
   in f pd' lbi uhs x

isPortableBuild :: String -> Bool
isPortableBuild "System.Process" = False
isPortableBuild _ = True

