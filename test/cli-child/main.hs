{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Main ( main ) where

-- base
import System.Environment
import System.IO
#if !defined(mingw32_HOST_OS)
import Control.Monad
  ( when )
#endif

-- deepseq
import Control.DeepSeq
  ( force )

#if !defined(mingw32_HOST_OS)
-- unix
import System.Posix.IO
  ( FdOption(NonBlockingRead), queryFdOption )
#endif

-- process
import System.Process.CommunicationHandle
  ( openCommunicationHandleRead
  , openCommunicationHandleWrite
  )
#if !defined(mingw32_HOST_OS)
import System.Process.CommunicationHandle.Internal
  ( CommunicationHandle(..) )
#endif

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
      input <- hGetContents hRead
      let !output = force $ reverse input ++ "123"
#if !defined(mingw32_HOST_OS)
      let CommunicationHandle fdWrite = read chWrite
      nonBlocking <- queryFdOption fdWrite NonBlockingRead
      when nonBlocking $
        error "cli-child: write end was made non-blocking by the parent"
#endif
      hWrite <- openCommunicationHandleWrite $ read chWrite
      hPutStr hWrite output
      putStrLn "cli-child }"
      hClose hWrite
    _ -> error $
      unlines [ "expected two CommunicationHandle arguments, but got:"
              , show args ]
