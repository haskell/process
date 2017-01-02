module Main where

import System.Process

main
 = do (_,_,_,p) <- createProcess_ "T9775_good" (proc "main" []{ use_process_jobs = True })
      waitForProcess p >>= print
