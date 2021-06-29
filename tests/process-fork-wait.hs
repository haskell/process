-- | This test verifies that the 'use_process_jobs' feature works as
-- advertised. Specifically: on Windows 'waitForProcess' should not return
-- until all processes created by the child (including those created with
-- @fork@) have exited if 'use_process_jobs' is enabled.
--

module Main where

import Control.Concurrent
import Control.Monad
import System.Environment
import System.IO
import System.Process

main :: IO ()
main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run [] = do
    putStrLn "starting A"
    hFlush stdout
    -- Disabling use_process_jobs here will cause waitForProcess to return
    -- before the process B invocation has written the test file.
    (_,_,_,p) <- createProcess $ (proc "process-fork-wait" ["A"]) { use_process_jobs = True }
    void $ waitForProcess p
    contents <- readFile "test"
    when (contents /= "looks good to me")
        $ fail "invalid file contents"
run ["A"] = do
    putStrLn "A started"
    hFlush stdout
    (_,_,_,_) <- createProcess $ (proc "process-fork-wait" ["B"])
    return ()
run ["B"] = do
    putStrLn "B started"
    hFlush stdout
    threadDelay (5*1000*1000)
    writeFile "test" "looks good to me"
    putStrLn "B finished"
run _ = fail "unknown mode"
