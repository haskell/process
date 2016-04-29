{-# LANGUAGE CPP, ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.Generic
-- Copyright   :  (c) The University of Glasgow 2004-2008
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires concurrency)
--
-- Generic implementation of 'readProcess' and related operations
-- shared by "System.Process" and "System.Process.Binary".
--
-----------------------------------------------------------------------------

module System.Process.Generic (
    shell, proc,
    withCreateProcess_,
    processFailedException,

    readCreateProcess,
    readProcess,
    readCreateProcessWithExitCode,
    readProcessWithExitCode,

    waitForProcess,
    getProcessExitCode,
    terminateProcess,
    ) where

import Prelude hiding (mapM, null)
import qualified Prelude as S (null)

import System.Process.Internals

import Control.Concurrent
import qualified Control.DeepSeq as DeepSeq
import Control.Exception (SomeException, mask, try, throwIO)
import qualified Control.Exception as C
import Control.Monad
import Foreign
import Foreign.C
import System.Exit      ( ExitCode(..) )
import System.IO hiding (hGetContents, hPutStr)
import qualified System.IO as S
import System.IO.Error (mkIOError)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
-- Needed to implement 'rnf' for ByteString.
#if !MIN_VERSION_bytestring(0,10,0)
import Data.ByteString.Lazy.Internal (ByteString(..))
#endif

-- Provide the data constructors for CPid on GHC 7.4 and later
#if !defined(WINDOWS) && MIN_VERSION_base(4,5,0)
import System.Posix.Types (CPid (..))
#endif

import GHC.IO.Exception ( IOErrorType(..), IOException(..) )

-- ----------------------------------------------------------------------------
-- createProcess

-- | Construct a 'CreateProcess' record for passing to 'createProcess',
-- representing a raw command with arguments.
--
-- See 'RawCommand' for precise semantics of the specified @FilePath@.
proc :: FilePath -> [String] -> CreateProcess
proc cmd args = CreateProcess { cmdspec = RawCommand cmd args,
                                cwd = Nothing,
                                env = Nothing,
                                std_in = Inherit,
                                std_out = Inherit,
                                std_err = Inherit,
                                close_fds = False,
                                create_group = False,
                                delegate_ctlc = False,
                                detach_console = False,
                                create_new_console = False,
                                new_session = False,
                                child_group = Nothing,
                                child_user = Nothing }

-- | Construct a 'CreateProcess' record for passing to 'createProcess',
-- representing a command to be passed to the shell.
shell :: String -> CreateProcess
shell str = CreateProcess { cmdspec = ShellCommand str,
                            cwd = Nothing,
                            env = Nothing,
                            std_in = Inherit,
                            std_out = Inherit,
                            std_err = Inherit,
                            close_fds = False,
                            create_group = False,
                            delegate_ctlc = False,
                            detach_console = False,
                            create_new_console = False,
                            new_session = False,
                            child_group = Nothing,
                            child_user = Nothing }

{-
-- TODO: decide if we want to expose this to users
-- | A 'C.bracketOnError'-style resource handler for 'createProcess'.
--
-- In normal operation it adds nothing, you are still responsible for waiting
-- for (or forcing) process termination and closing any 'Handle's. It only does
-- automatic cleanup if there is an exception. If there is an exception in the
-- body then it ensures that the process gets terminated and any 'CreatePipe'
-- 'Handle's are closed. In particular this means that if the Haskell thread
-- is killed (e.g. 'killThread'), that the external process is also terminated.
--
-- e.g.
--
-- > withCreateProcess (proc cmd args) { ... }  $ \_ _ _ ph -> do
-- >   ...
--
withCreateProcess
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess c action =
    C.bracketOnError (createProcess c) cleanupProcess
                     (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)
-}

-- wrapper so we can get exceptions with the appropriate function name.
withCreateProcess_
  :: String
  -> CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess_ fun c action =
    C.bracketOnError (createProcess_ fun c) cleanupProcess
                     (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)


cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
               -> IO ()
cleanupProcess (mb_stdin, mb_stdout, mb_stderr,
                ph@(ProcessHandle _ delegating_ctlc)) = do
    terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
    maybe (return ()) hClose mb_stdout
    maybe (return ()) hClose mb_stderr
    -- terminateProcess does not guarantee that it terminates the process.
    -- Indeed on Unix it's SIGTERM, which asks nicely but does not guarantee
    -- that it stops. If it doesn't stop, we don't want to hang, so we wait
    -- asynchronously using forkIO.

    -- However we want to end the Ctl-C handling synchronously, so we'll do
    -- that synchronously, and set delegating_ctlc as False for the
    -- waitForProcess (which would otherwise end the Ctl-C delegation itself).
    when delegating_ctlc
      stopDelegateControlC
    _ <- forkIO (waitForProcess (resetCtlcDelegation ph) >> return ())
    return ()
  where
    resetCtlcDelegation (ProcessHandle m _) = ProcessHandle m False


processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fun cmd args exit_code =
      ioError (mkIOError OtherError (fun ++ ": " ++ cmd ++
                                     concatMap ((' ':) . show) args ++
                                     " (exit " ++ show exit_code ++ ")")
                                 Nothing Nothing)


-- -----------------------------------------------------------------------------
-- readProcess

{- The following are generic implementations of readProcess,
readCreateProcess, readProcessWithExitCode, and
readCreateProcessWithExitCode that works for both String and lazy
ByteString. See System.Process for the API documentation.
-}

-- This internal type class encapsulates the parts of our
-- readProcess (et al.) implementation that differ depending
-- on the string type.
class StringIO s where
  isBinaryString :: s -> Bool
  null :: s -> Bool
  rnf :: s -> ()
  hGetContents :: Handle -> IO s
  hPutStr :: Handle -> s -> IO ()

-- Polymorphic 'CreatePipe' constructor: creates the pipe in text or binary
-- mode according to whether 's' is a text or binary string type.
polyCreatePipe :: StringIO s => s -> StdStream
polyCreatePipe s | isBinaryString s = CreateBinaryPipe
                 | otherwise = CreatePipe

instance StringIO [Char] where
  isBinaryString _ = False
  null = S.null
  rnf = DeepSeq.rnf
  hGetContents = S.hGetContents
  hPutStr = S.hPutStr

instance StringIO ByteString where
  isBinaryString _ = True
  null = B.null
  
-- The NFData instance for ByteString was added to the bytestring package
-- in version 0.10.0.
#if MIN_VERSION_bytestring(0,10,0)
  rnf = DeepSeq.rnf
#else
  rnf Empty       = ()
  rnf (Chunk _ b) = rnf b
#endif

  hGetContents = B.hGetContents
  hPutStr = B.hPutStr


readProcess :: StringIO s => FilePath -> [String] -> s -> IO s
readProcess cmd args = readCreateProcess $ proc cmd args

readCreateProcess :: StringIO s => CreateProcess -> s -> IO s
readCreateProcess cp input = do
    let cp_opts = cp {
                    std_in  = polyCreatePipe input,
                    std_out = polyCreatePipe input
                  }
    (ex, output) <- withCreateProcess_ "readCreateProcess" cp_opts $
      \(Just inh) (Just outh) _ ph -> do

        -- fork off a thread to start consuming the output
        output  <- hGetContents outh
        withForkWait (C.evaluate $ rnf output) $ \waitOut -> do

          -- now write any input
          unless (null input) $
            ignoreSigPipe $ hPutStr inh input
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

readProcessWithExitCode :: StringIO s => FilePath -> [String] -> s -> IO (ExitCode,s,s)
readProcessWithExitCode cmd args =
    readCreateProcessWithExitCode $ proc cmd args

readCreateProcessWithExitCode :: StringIO s => CreateProcess -> s -> IO (ExitCode,s,s)
readCreateProcessWithExitCode cp input = do
    let cp_opts = cp {
                    std_in  = polyCreatePipe input,
                    std_out = polyCreatePipe input,
                    std_err = polyCreatePipe input
                  }
    withCreateProcess_ "readCreateProcessWithExitCode" cp_opts $
      \(Just inh) (Just outh) (Just errh) ph -> do

        out <- hGetContents outh
        err <- hGetContents errh

        -- fork off threads to start consuming stdout & stderr
        withForkWait  (C.evaluate $ rnf out) $ \waitOut ->
         withForkWait (C.evaluate $ rnf err) $ \waitErr -> do

          -- now write any input
          unless (null input) $
            ignoreSigPipe $ hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut
          waitErr

          hClose outh
          hClose errh

        -- wait on the process
        ex <- waitForProcess ph

        return (ex, out, err)


-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `C.onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e


-- ----------------------------------------------------------------------------
-- waitForProcess

{- | Waits for the specified process to terminate, and returns its exit code.

GHC Note: in order to call @waitForProcess@ without blocking all the
other threads in the system, you must compile the program with
@-threaded@.

(/Since: 1.2.0.0/) On Unix systems, a negative value @'ExitFailure' -/signum/@
indicates that the child was terminated by signal @/signum/@.
The signal numbers are platform-specific, so to test for a specific signal use
the constants provided by "System.Posix.Signals" in the @unix@ package.
Note: core dumps are not reported, use "System.Posix.Process" if you need this
detail.

-}
waitForProcess
  :: ProcessHandle
  -> IO ExitCode
waitForProcess ph@(ProcessHandle _ delegating_ctlc) = do
  p_ <- modifyProcessHandle ph $ \p_ -> return (p_,p_)
  case p_ of
    ClosedHandle e -> return e
    OpenHandle h  -> do
        -- don't hold the MVar while we call c_waitForProcess...
        -- (XXX but there's a small race window here during which another
        -- thread could close the handle or call waitForProcess)
        e <- alloca $ \pret -> do
          throwErrnoIfMinus1Retry_ "waitForProcess" (c_waitForProcess h pret)
          modifyProcessHandle ph $ \p_' ->
            case p_' of
              ClosedHandle e -> return (p_',e)
              OpenHandle ph' -> do
                closePHANDLE ph'
                code <- peek pret
                let e = if (code == 0)
                       then ExitSuccess
                       else (ExitFailure (fromIntegral code))
                return (ClosedHandle e, e)
        when delegating_ctlc $
          endDelegateControlC e
        return e


-- ----------------------------------------------------------------------------
-- getProcessExitCode

{- |
This is a non-blocking version of 'waitForProcess'.  If the process is
still running, 'Nothing' is returned.  If the process has exited, then
@'Just' e@ is returned where @e@ is the exit code of the process.

On Unix systems, see 'waitForProcess' for the meaning of exit codes
when the process died as the result of a signal.
-}

getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode ph@(ProcessHandle _ delegating_ctlc) = do
  (m_e, was_open) <- modifyProcessHandle ph $ \p_ ->
    case p_ of
      ClosedHandle e -> return (p_, (Just e, False))
      OpenHandle h ->
        alloca $ \pExitCode -> do
            res <- throwErrnoIfMinus1Retry "getProcessExitCode" $
                        c_getProcessExitCode h pExitCode
            code <- peek pExitCode
            if res == 0
              then return (p_, (Nothing, False))
              else do
                   closePHANDLE h
                   let e  | code == 0 = ExitSuccess
                          | otherwise = ExitFailure (fromIntegral code)
                   return (ClosedHandle e, (Just e, True))
  case m_e of
    Just e | was_open && delegating_ctlc -> endDelegateControlC e
    _                                    -> return ()
  return m_e


-- ----------------------------------------------------------------------------
-- terminateProcess

-- | Attempts to terminate the specified process.  This function should
-- not be used under normal circumstances - no guarantees are given regarding
-- how cleanly the process is terminated.  To check whether the process
-- has indeed terminated, use 'getProcessExitCode'.
--
-- On Unix systems, 'terminateProcess' sends the process the SIGTERM signal.
-- On Windows systems, the Win32 @TerminateProcess@ function is called, passing
-- an exit code of 1.
--
-- Note: on Windows, if the process was a shell command created by
-- 'createProcess' with 'shell', or created by 'runCommand' or
-- 'runInteractiveCommand', then 'terminateProcess' will only
-- terminate the shell, not the command itself.  On Unix systems, both
-- processes are in a process group and will be terminated together.

terminateProcess :: ProcessHandle -> IO ()
terminateProcess ph = do
  withProcessHandle ph $ \p_ ->
    case p_ of
      ClosedHandle _ -> return ()
      OpenHandle h -> do
        throwErrnoIfMinus1Retry_ "terminateProcess" $ c_terminateProcess h
        return ()
        -- does not close the handle, we might want to try terminating it
        -- again, or get its exit code.


-- ----------------------------------------------------------------------------
-- Interface to C bits

foreign import ccall unsafe "terminateProcess"
  c_terminateProcess
        :: PHANDLE
        -> IO CInt

foreign import ccall unsafe "getProcessExitCode"
  c_getProcessExitCode
        :: PHANDLE
        -> Ptr CInt
        -> IO CInt

foreign import ccall interruptible "waitForProcess" -- NB. safe - can block
  c_waitForProcess
        :: PHANDLE
        -> Ptr CInt
        -> IO CInt
