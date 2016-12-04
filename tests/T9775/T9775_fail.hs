module Main where

import System.Process

main
 = do (_,_,_,p) <- createProcess_ "T9775_fail" (proc "main" "")
      waitForProcess p >> print
      