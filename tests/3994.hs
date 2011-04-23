module Main where

import Control.Concurrent
import System.IO
import System.Process

main :: IO ()
main = do (_,_,_,p) <- createProcess (proc "sleep" ["10"])
                            { std_out = CreatePipe, create_group = True }
          interruptProcessGroupOf p
          t <- myThreadId
               -- timeout
          forkIO $ do
            threadDelay 5000000
            killThread t
          waitForProcess p
          putStrLn "end"
          return ()

