{-# LANGUAGE CPP, ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE InterruptibleFFI #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process
-- Copyright   :  (c) The University of Glasgow 2004-2008
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires concurrency)
--
-- Operations for creating and interacting with sub-processes.
--
-----------------------------------------------------------------------------

-- ToDo:
--      * Flag to control whether exiting the parent also kills the child.

{- NOTES on createPipe:
 
   createPipe is no longer exported, because of the following problems:

        - it wasn't used to implement runInteractiveProcess on Unix, because
          the file descriptors for the unused ends of the pipe need to be closed
          in the child process.

        - on Windows, a special version of createPipe is needed that sets
          the inheritance flags correctly on the ends of the pipe (see
          mkAnonPipe below).
-}

module System.Process (
#ifndef __HUGS__
        -- * Running sub-processes
        createProcess,
        shell, proc,
        CreateProcess(..),
        CmdSpec(..),
        StdStream(..),
        ProcessHandle,

        -- ** Specific variants of createProcess
        runCommand,
        runProcess,
        runInteractiveCommand,
        runInteractiveProcess,
        readProcess,
        readProcessWithExitCode,
#endif
        system,
        rawSystem,
        showCommandForUser,

#ifndef __HUGS__
        -- * Process completion
        waitForProcess,
        getProcessExitCode,
        terminateProcess,
        interruptProcessGroupOf,
#endif
 ) where

import Prelude hiding (mapM)

#ifndef __HUGS__
import System.Process.Internals

import System.IO.Error
#if !defined(mingw32_HOST_OS)
import System.Posix.Types
#if MIN_VERSION_unix(2,5,0)
import System.Posix.Process (getProcessGroupIDOf)
#endif
#endif
import qualified Control.Exception as C
import Control.Concurrent
import Control.Monad
import Foreign
import Foreign.C
import System.IO
import Data.Maybe
#endif
import System.Exit      ( ExitCode(..) )

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO.Exception ( ioException, IOErrorType(..) )
#else
import GHC.IOBase       ( ioException, IOErrorType(..) )
#endif
#if defined(mingw32_HOST_OS)
import System.Win32.Process (getProcessId)
import System.Win32.Console (generateConsoleCtrlEvent, cTRL_BREAK_EVENT)
#else
import System.Posix.Signals
#endif
#endif

#ifdef __HUGS__
import Hugs.System
#endif

#ifdef __NHC__
import System (system)
#endif


#ifndef __HUGS__
-- ----------------------------------------------------------------------------
-- runCommand

{- | Runs a command using the shell.
 -}
runCommand
  :: String
  -> IO ProcessHandle

runCommand string = do
  (_,_,_,ph) <- runGenProcess_ "runCommand" (shell string) Nothing Nothing
  return ph

-- ----------------------------------------------------------------------------
-- runProcess

{- | Runs a raw command, optionally specifying 'Handle's from which to
     take the @stdin@, @stdout@ and @stderr@ channels for the new
     process (otherwise these handles are inherited from the current
     process).

     Any 'Handle's passed to 'runProcess' are placed immediately in the 
     closed state.

     Note: consider using the more general 'createProcess' instead of
     'runProcess'.
-}
runProcess
  :: FilePath                   -- ^ Filename of the executable
  -> [String]                   -- ^ Arguments to pass to the executable
  -> Maybe FilePath             -- ^ Optional path to the working directory
  -> Maybe [(String,String)]    -- ^ Optional environment (otherwise inherit)
  -> Maybe Handle               -- ^ Handle to use for @stdin@ (Nothing => use existing @stdin@)
  -> Maybe Handle               -- ^ Handle to use for @stdout@ (Nothing => use existing @stdout@)
  -> Maybe Handle               -- ^ Handle to use for @stderr@ (Nothing => use existing @stderr@)
  -> IO ProcessHandle

runProcess cmd args mb_cwd mb_env mb_stdin mb_stdout mb_stderr = do
  (_,_,_,ph) <-
      runGenProcess_ "runProcess"
         (proc cmd args){ cwd = mb_cwd,
                          env = mb_env,
                          std_in  = mbToStd mb_stdin,
                          std_out = mbToStd mb_stdout,
                          std_err = mbToStd mb_stderr }
          Nothing Nothing
  maybeClose mb_stdin
  maybeClose mb_stdout
  maybeClose mb_stderr
  return ph
 where
  maybeClose :: Maybe Handle -> IO ()
  maybeClose (Just  hdl)
    | hdl /= stdin && hdl /= stdout && hdl /= stderr = hClose hdl
  maybeClose _ = return ()

  mbToStd :: Maybe Handle -> StdStream
  mbToStd Nothing    = Inherit
  mbToStd (Just hdl) = UseHandle hdl

-- ----------------------------------------------------------------------------
-- createProcess

-- | Construct a 'CreateProcess' record for passing to 'createProcess',
-- representing a raw command with arguments.
proc :: FilePath -> [String] -> CreateProcess
proc cmd args = CreateProcess { cmdspec = RawCommand cmd args,
                                cwd = Nothing,
                                env = Nothing,
                                std_in = Inherit,
                                std_out = Inherit,
                                std_err = Inherit,
                                close_fds = False,
                                create_group = False}

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
                            create_group = False}

{- |
This is the most general way to spawn an external process.  The
process can be a command line to be executed by a shell or a raw command
with a list of arguments.  The stdin, stdout, and stderr streams of
the new process may individually be attached to new pipes, to existing
'Handle's, or just inherited from the parent (the default.)

The details of how to create the process are passed in the
'CreateProcess' record.  To make it easier to construct a
'CreateProcess', the functions 'proc' and 'shell' are supplied that
fill in the fields with default values which can be overriden as
needed.

'createProcess' returns @(mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, p)@,
where 

 * if @std_in == CreatePipe@, then @mb_stdin_hdl@ will be @Just h@,
   where @h@ is the write end of the pipe connected to the child
   process's @stdin@.

 * otherwise, @mb_stdin_hdl == Nothing@

Similarly for @mb_stdout_hdl@ and @mb_stderr_hdl@.

For example, to execute a simple @ls@ command:

>   r <- createProcess (proc "ls" [])

To create a pipe from which to read the output of @ls@:

>   (_, Just hout, _, _) <-
>       createProcess (proc "ls" []){ std_out = CreatePipe }

To also set the directory in which to run @ls@:

>   (_, Just hout, _, _) <-
>       createProcess (proc "ls" []){ cwd = Just "\home\bob",
>                                     std_out = CreatePipe }

-}
createProcess
  :: CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = do
  r <- runGenProcess_ "createProcess" cp Nothing Nothing
  maybeCloseStd (std_in  cp)
  maybeCloseStd (std_out cp)
  maybeCloseStd (std_err cp)
  return r
 where
  maybeCloseStd :: StdStream -> IO ()
  maybeCloseStd (UseHandle hdl)
    | hdl /= stdin && hdl /= stdout && hdl /= stderr = hClose hdl
  maybeCloseStd _ = return ()

-- ----------------------------------------------------------------------------
-- runInteractiveCommand

{- | Runs a command using the shell, and returns 'Handle's that may
     be used to communicate with the process via its @stdin@, @stdout@,
     and @stderr@ respectively. The 'Handle's are initially in binary
     mode; if you need them to be in text mode then use 'hSetBinaryMode'.
-}
runInteractiveCommand
  :: String
  -> IO (Handle,Handle,Handle,ProcessHandle)

runInteractiveCommand string =
  runInteractiveProcess1 "runInteractiveCommand" (shell string)

-- ----------------------------------------------------------------------------
-- runInteractiveProcess

{- | Runs a raw command, and returns 'Handle's that may be used to communicate
     with the process via its @stdin@, @stdout@ and @stderr@ respectively.

    For example, to start a process and feed a string to its stdin:
   
>   (inp,out,err,pid) <- runInteractiveProcess "..."
>   forkIO (hPutStr inp str)

    The 'Handle's are initially in binary mode; if you need them to be
    in text mode then use 'hSetBinaryMode'.
-}
runInteractiveProcess
  :: FilePath                   -- ^ Filename of the executable
  -> [String]                   -- ^ Arguments to pass to the executable
  -> Maybe FilePath             -- ^ Optional path to the working directory
  -> Maybe [(String,String)]    -- ^ Optional environment (otherwise inherit)
  -> IO (Handle,Handle,Handle,ProcessHandle)

runInteractiveProcess cmd args mb_cwd mb_env = do
  runInteractiveProcess1 "runInteractiveProcess" 
        (proc cmd args){ cwd = mb_cwd, env = mb_env }

runInteractiveProcess1
  :: String
  -> CreateProcess
  -> IO (Handle,Handle,Handle,ProcessHandle)
runInteractiveProcess1 fun cmd = do
  (mb_in, mb_out, mb_err, p) <- 
      runGenProcess_ fun
           cmd{ std_in  = CreatePipe,
                std_out = CreatePipe,
                std_err = CreatePipe } 
           Nothing Nothing
  return (fromJust mb_in, fromJust mb_out, fromJust mb_err, p)

-- ----------------------------------------------------------------------------
-- waitForProcess

{- | Waits for the specified process to terminate, and returns its exit code.
   
     GHC Note: in order to call @waitForProcess@ without blocking all the
     other threads in the system, you must compile the program with
     @-threaded@.
-}
waitForProcess
  :: ProcessHandle
  -> IO ExitCode
waitForProcess ph = do
  p_ <- withProcessHandle ph $ \p_ -> return (p_,p_)
  case p_ of
    ClosedHandle e -> return e
    OpenHandle h  -> do
        -- don't hold the MVar while we call c_waitForProcess...
        -- (XXX but there's a small race window here during which another
        -- thread could close the handle or call waitForProcess)
        alloca $ \pret -> do
          throwErrnoIfMinus1Retry_ "waitForProcess" (c_waitForProcess h pret)
          withProcessHandle ph $ \p_' ->
            case p_' of
              ClosedHandle e -> return (p_',e)
              OpenHandle ph' -> do
                closePHANDLE ph'
                code <- peek pret
                let e = if (code == 0)
                       then ExitSuccess
                       else (ExitFailure (fromIntegral code))
                return (ClosedHandle e, e)

-- -----------------------------------------------------------------------------
--
-- | readProcess forks an external process, reads its standard output
-- strictly, blocking until the process terminates, and returns the output
-- string.
--
-- Output is returned strictly, so this is not suitable for
-- interactive applications.
--
-- This function throws an 'IOError' if the process 'ExitCode' is
-- anything other than 'ExitSuccess'.
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
-- * The command to run, which must be in the $PATH, or an absolute path
--
-- * A list of separate command line arguments to the program
--
-- * A string to pass on the standard input to the program.
--
readProcess 
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readProcess cmd args input = do
    (Just inh, Just outh, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = Inherit }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ C.evaluate (length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> 
      ioError (mkIOError OtherError ("readProcess: " ++ cmd ++ 
                                     ' ':unwords (map show args) ++ 
                                     " (exit " ++ show r ++ ")")
                                 Nothing Nothing)

{- |
readProcessWithExitCode creates an external process, reads its
standard output and standard error strictly, waits until the process
terminates, and then returns the 'ExitCode' of the process,
the standard output, and the standard error.

'readProcess' and 'readProcessWithExitCode' are fairly simple wrappers
around 'createProcess'.  Constructing variants of these functions is
quite easy: follow the link to the source code to see how
'readProcess' is implemented.
-}

readProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- hGetContents outh
    _ <- forkIO $ C.evaluate (length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- hGetContents errh
    _ <- forkIO $ C.evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    hClose errh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)
#endif /* !__HUGS__ */

-- ---------------------------------------------------------------------------
-- system

{-| 
Computation @system cmd@ returns the exit code produced when the
operating system runs the shell command @cmd@.

This computation may fail with

   * @PermissionDenied@: The process has insufficient privileges to
     perform the operation.

   * @ResourceExhausted@: Insufficient resources are available to
     perform the operation.

   * @UnsupportedOperation@: The implementation does not support
     system calls.

On Windows, 'system' passes the command to the Windows command
interpreter (@CMD.EXE@ or @COMMAND.COM@), hence Unixy shell tricks
will not work.
-}
#ifdef __GLASGOW_HASKELL__
system :: String -> IO ExitCode
system "" = ioException (ioeSetErrorString (mkIOError InvalidArgument "system" Nothing Nothing) "null command")
system str = syncProcess "system" (shell str)


syncProcess :: String -> CreateProcess -> IO ExitCode
#if mingw32_HOST_OS
syncProcess _fun c = do
  (_,_,_,p) <- createProcess c
  waitForProcess p
#else
syncProcess fun c = do
  -- The POSIX version of system needs to do some manipulation of signal
  -- handlers.  Since we're going to be synchronously waiting for the child,
  -- we want to ignore ^C in the parent, but handle it the default way
  -- in the child (using SIG_DFL isn't really correct, it should be the
  -- original signal handler, but the GHC RTS will have already set up
  -- its own handler and we don't want to use that).
  old_int  <- installHandler sigINT  Ignore Nothing
  old_quit <- installHandler sigQUIT Ignore Nothing
  (_,_,_,p) <- runGenProcess_ fun c
                (Just defaultSignal) (Just defaultSignal)
  r <- waitForProcess p
  _ <- installHandler sigINT  old_int Nothing
  _ <- installHandler sigQUIT old_quit Nothing
  return r
#endif  /* mingw32_HOST_OS */
#endif  /* __GLASGOW_HASKELL__ */

{-|
The computation @'rawSystem' cmd args@ runs the operating system command
@cmd@ in such a way that it receives as arguments the @args@ strings
exactly as given, with no funny escaping or shell meta-syntax expansion.
It will therefore behave more portably between operating systems than 'system'.

The return codes and possible failures are the same as for 'system'.
-}
rawSystem :: String -> [String] -> IO ExitCode
#ifdef __GLASGOW_HASKELL__
rawSystem cmd args = syncProcess "rawSystem" (proc cmd args)
#elif !mingw32_HOST_OS
-- crude fallback implementation: could do much better than this under Unix
rawSystem cmd args = system (showCommandForUser cmd args)
#else /* mingw32_HOST_OS &&  ! __GLASGOW_HASKELL__ */
# if __HUGS__
rawSystem cmd args = system (cmd ++ showCommandForUser "" args)
# else
rawSystem cmd args = system (showCommandForUser cmd args)
#endif
#endif

-- | Given a program @p@ and arguments @args@,
--   @showCommandForUser p args@ returns a string suitable for pasting
--   into sh (on POSIX OSs) or cmd.exe (on Windows).
showCommandForUser :: FilePath -> [String] -> String
showCommandForUser cmd args = unwords (map translate (cmd : args))

#ifndef __HUGS__
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
  withProcessHandle_ ph $ \p_ ->
    case p_ of 
      ClosedHandle _ -> return p_
      OpenHandle h -> do
        throwErrnoIfMinus1Retry_ "terminateProcess" $ c_terminateProcess h
        return p_
        -- does not close the handle, we might want to try terminating it
        -- again, or get its exit code.

-- ----------------------------------------------------------------------------
-- interruptProcessGroupOf

-- | Sends an interrupt signal to the process group of the given process.
--
-- On Unix systems, it sends the group the SIGINT signal.
--
-- On Windows systems, it generates a CTRL_BREAK_EVENT and will only work for
-- processes created using 'createProcess' and setting the 'create_group' flag

interruptProcessGroupOf
    :: ProcessHandle    -- ^ A process in the process group
    -> IO ()
interruptProcessGroupOf ph = do
#if mingw32_HOST_OS
    withProcessHandle_ ph $ \p_ -> do
        case p_ of
            ClosedHandle _ -> return p_
            OpenHandle h -> do
                pid <- getProcessId h
                generateConsoleCtrlEvent cTRL_BREAK_EVENT pid
                return p_
#else
    withProcessHandle_ ph $ \p_ -> do
        case p_ of
            ClosedHandle _ -> return p_
            OpenHandle h -> do
#if MIN_VERSION_unix(2,5,0)
                -- getProcessGroupIDOf was added in unix-2.5.0.0
                pgid <- getProcessGroupIDOf h
                signalProcessGroup sigINT pgid
#else
                signalProcessGroup sigINT h
#endif
                return p_
#endif

-- ----------------------------------------------------------------------------
-- getProcessExitCode

{- | 
This is a non-blocking version of 'waitForProcess'.  If the process is
still running, 'Nothing' is returned.  If the process has exited, then
@'Just' e@ is returned where @e@ is the exit code of the process.
-}
getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode ph = do
  withProcessHandle ph $ \p_ ->
    case p_ of
      ClosedHandle e -> return (p_, Just e)
      OpenHandle h ->
        alloca $ \pExitCode -> do
            res <- throwErrnoIfMinus1Retry "getProcessExitCode" $
                        c_getProcessExitCode h pExitCode
            code <- peek pExitCode
            if res == 0
              then return (p_, Nothing)
              else do
                   closePHANDLE h
                   let e  | code == 0 = ExitSuccess
                          | otherwise = ExitFailure (fromIntegral code)
                   return (ClosedHandle e, Just e)

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

#if __GLASGOW_HASKELL__ < 701
-- not available prior to 7.1
#define interruptible safe
#endif

foreign import ccall interruptible "waitForProcess" -- NB. safe - can block
  c_waitForProcess
        :: PHANDLE
        -> Ptr CInt
        -> IO CInt
#endif /* !__HUGS__ */

