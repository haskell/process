{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Main ( main ) where

-- base
import System.Environment
import System.IO

-- deepseq
import Control.DeepSeq
  ( force )

-- process
import System.Process.CommunicationHandle
  ( openCommunicationHandleRead
  , openCommunicationHandleWrite
  )

#if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem ((<!>))
#endif

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ chRead, chWrite ] -> do
      childUsesWinIO <-
        return False
#if defined(__IO_MANAGER_WINIO__)
          <!> return True
#endif
      putStr $ unlines
        [ "cli-child {"
        , " childUsesWinIO: " ++ show childUsesWinIO ]
      hRead  <- openCommunicationHandleRead  $ read chRead
      hWrite <- openCommunicationHandleWrite $ read chWrite
      input <- hGetContents hRead
      let !output = force $ reverse input ++ "123"
      hPutStr hWrite output
      putStrLn "cli-child }"
      hClose hWrite
    _ -> error $
      unlines [ "expected two CommunicationHandle arguments, but got:"
              , show args ]
