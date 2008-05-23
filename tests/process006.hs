module Main where

import Control.Concurrent
import System.IO
import System.Process
import Control.Monad
import Control.Exception

main :: IO ()
main = do 
  print =<< readProcess "cat" [] "yan\ntan\tether\n"
  print =<< readProcessWithExitCode "cat" [] "yan\ntan\tether\n"
  print =<< readProcessWithExitCode "ls" ["/does/not/exist"] ""
  print =<< (try $ readProcess "ls" ["/does/not/exist"] "")
