module Main where

import System.Process
import System.Process.Internals
import System.Exit

main
 = do (_,_,_,_,Just j,Just io) <- createProcessExt_ "T9775_good" True (proc "main" [])
      maybe (ExitFailure (-7)) mkExitCode <$> waitForJobCompletion j io 0xFFFFFFFF >>= print
        where mkExitCode code | code == 0 = ExitSuccess
                              | otherwise = ExitFailure $ fromIntegral code
      