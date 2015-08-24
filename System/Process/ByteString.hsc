{-# LANGUAGE CPP, ForeignFunctionInterface #-}
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE InterruptibleFFI #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.ByteString
-- Copyright   :  (c) The University of Glasgow 2004-2015
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires concurrency)
--
-- Operations for creating and interacting with sub-processes.
--
-----------------------------------------------------------------------------

module System.Process.ByteString (
    -- ** Simpler functions for common tasks
    readCreateProcess,
    readProcess,
    readCreateProcessWithExitCode,
    readProcessWithExitCode,
) where

import System.Process (proc)

import Prelude hiding (mapM)

import System.Process.Internals

import Control.Concurrent
import qualified Control.Exception as C
import Control.Monad
import System.Exit      ( ExitCode(..) )
import System.IO

import qualified Data.ByteString as BS

-- -----------------------------------------------------------------------------

-- | @readProcess@ forks an external process, reads its standard output
-- strictly, blocking until the process terminates, and returns the output
-- string. The external process inherits the standard error.
--
-- If an asynchronous exception is thrown to the thread executing
-- @readProcess@, the forked process will be terminated and @readProcess@ will
-- wait (block) until the process has been terminated.
--
-- Output is returned strictly, so this is not suitable for
-- interactive applications.
--
-- This function throws an 'IOError' if the process 'ExitCode' is
-- anything other than 'ExitSuccess'. If instead you want to get the
-- 'ExitCode' then use 'readProcessWithExitCode'.
--
-- Users of this function should compile with @-threaded@ if they
-- want other Haskell threads to keep running while waiting on
-- the result of readProcess.
--
-- >  > readProcess "date" [] []
-- >  "Thu Feb  7 10:03:39 PST 2008\n"
--
-- The arguments are:
--
-- * The command to run, which must be in the $PATH, or an absolute or relative path
--
-- * A list of separate command line arguments to the program
--
-- * A string to pass on standard input to the forked process.
--
readProcess
    :: FilePath                 -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [String]                 -- ^ any arguments
    -> BS.ByteString            -- ^ standard input
    -> IO BS.ByteString         -- ^ stdout
readProcess cmd args = readCreateProcess $ proc cmd args

-- | @readCreateProcess@ works exactly like 'readProcess' except that it
-- lets you pass 'CreateProcess' giving better flexibility.
--
-- >  > readCreateProcess (shell "pwd" { cwd = "/etc/" }) ""
-- >  "/etc\n"
--
-- Note that @Handle@s provided for @std_in@ or @std_out@ via the CreateProcess
-- record will be ignored.
-- /Since: 1.2.3.0/

readCreateProcess
    :: CreateProcess
    -> BS.ByteString            -- ^ standard input
    -> IO BS.ByteString         -- ^ stdout
readCreateProcess cp input = do
    let cp_opts = cp {
                    std_in  = CreatePipe,
                    std_out = CreatePipe
                  }
    (ex, output) <- withCreateProcess_ "readCreateProcess" cp_opts $
      \(Just inh) (Just outh) _ ph -> do

        -- fork off a thread to start consuming the output
        output  <- BS.hGetContents outh
        withForkWait (C.evaluate output >> return ()) $ \waitOut -> do

          -- now write any input
          unless (BS.null input) $
            ignoreSigPipe $ BS.hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut
          hClose outh

        -- wait on the process
        ex <- waitForProcess ph
        return (ex, output)

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> processFailedException "readCreateProcess" cmd args r
  where
    cmd = case cp of
            CreateProcess { cmdspec = ShellCommand sc } -> sc
            CreateProcess { cmdspec = RawCommand fp _ } -> fp
    args = case cp of
             CreateProcess { cmdspec = ShellCommand _ } -> []
             CreateProcess { cmdspec = RawCommand _ args' } -> args'


-- | @readProcessWithExitCode@ is like @readProcess@ but with two differences:
--
--  * it returns the 'ExitCode' of the process, and does not throw any
--    exception if the code is not 'ExitSuccess'.
--
--  * it reads and returns the output from process' standard error handle,
--    rather than the process inheriting the standard error handle.
--
-- On Unix systems, see 'waitForProcess' for the meaning of exit codes
-- when the process died as the result of a signal.
--
readProcessWithExitCode
    :: FilePath                                  -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [String]                                  -- ^ any arguments
    -> BS.ByteString                             -- ^ standard input
    -> IO (ExitCode,BS.ByteString,BS.ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args =
    readCreateProcessWithExitCode $ proc cmd args

-- | @readCreateProcessWithExitCode@ works exactly like 'readProcessWithExitCode' except that it
-- lets you pass 'CreateProcess' giving better flexibility.
--
-- Note that @Handle@s provided for @std_in@, @std_out@, or @std_err@ via the CreateProcess
-- record will be ignored.
--
-- /Since: 1.2.3.0/
readCreateProcessWithExitCode
    :: CreateProcess
    -> BS.ByteString                             -- ^ standard input
    -> IO (ExitCode,BS.ByteString,BS.ByteString) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode cp input = do
    let cp_opts = cp {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = CreatePipe
                  }
    withCreateProcess_ "readCreateProcessWithExitCode" cp_opts $
      \(Just inh) (Just outh) (Just errh) ph -> do

        outVar <- newEmptyMVar
        errVar <- newEmptyMVar

        -- fork off threads to start consuming stdout & stderr
        withForkWait  (BS.hGetContents outh >>= putMVar outVar) $ \waitOut ->
         withForkWait (BS.hGetContents errh >>= putMVar errVar) $ \waitErr -> do

          -- now write any input
          unless (BS.null input) $
            ignoreSigPipe $ BS.hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut
          waitErr

          hClose outh
          hClose errh

        -- wait on the process
        ex <- waitForProcess ph

        out <- takeMVar outVar
        err <- takeMVar errVar

        return (ex, out, err)

