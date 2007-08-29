module Main (main) where

import Data.List
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

main :: IO ()
main = do 
          let hooks = defaultUserHooks {
                  buildHook = filter_modules_hook
                            $ buildHook defaultUserHooks,
                  makefileHook = filter_modules_hook
                               $ makefileHook defaultUserHooks,
                  instHook = filter_modules_hook
                           $ instHook defaultUserHooks }
          defaultMainWithHooks hooks

type Hook a = PackageDescription -> LocalBuildInfo -> UserHooks -> a -> IO ()

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
