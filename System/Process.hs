{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
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
    -- * Running sub-processes
    createProcess,
    shell, proc,
    CreateProcess(..),
    CmdSpec(..),
    StdStream(..),
    ProcessHandle,

    -- ** Simpler functions for common tasks
    callProcess,
    callCommand,
    spawnProcess,
    spawnCommand,
    readProcess,
    readProcessWithExitCode,

    -- ** Related utilities
    showCommandForUser,

    -- ** Control-C handling on Unix
    -- $ctlc-handling

    -- * Process completion
    waitForProcess,
    getProcessExitCode,
    terminateProcess,
    interruptProcessGroupOf,

    -- * Old deprecated functions
    -- | These functions pre-date 'createProcess' which is much more
    -- flexible.
    runProcess,
    runCommand,
    runInteractiveProcess,
    runInteractiveCommand,
    system,
    rawSystem,
    ) where

import Prelude hiding (mapM)

import System.Process.Internals

import Control.Concurrent
import Control.DeepSeq (rnf)
import Control.Exception (SomeException, mask, try, throwIO)
import qualified Control.Exception as C
import Control.Monad
import Data.Maybe
import Foreign
import Foreign.C
import System.Exit      ( ExitCode(..) )
import System.IO
import System.IO.Error (mkIOError, ioeSetErrorString)

#if !defined(mingw32_HOST_OS)
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Types
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.IO.Exception ( ioException, IOErrorType(..), IOException(..) )
# if defined(mingw32_HOST_OS)
import System.Win32.Console (generateConsoleCtrlEvent, cTRL_BREAK_EVENT)
import System.Win32.Process (getProcessId)
# else
import System.Posix.Signals
# endif
#endif

-- ----------------------------------------------------------------------------
-- createProcess

-- | Construct a 'CreateProcess' record for passing to 'createProcess',
-- representing a raw command with arguments.
--
-- The 'FilePath' argument names the executable, and is interpreted according
-- to the platform's standard policy for searching for
-- executables. Specifically:
--
-- * on Unix systems the
--   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/execvp.html execvp(3)>
--   semantics is used, where if the executable filename does not
--   contain a slash (@/@) then the @PATH@ environment variable is
--   searched for the executable.
--
-- * on Windows systems the Win32 @CreateProcess@ semantics is used.
--   Briefly: if the filename does not contain a path, then the
--   directory containing the parent executable is searched, followed
--   by the current directory, then some standard locations, and
--   finally the current @PATH@.  An @.exe@ extension is added if the
--   filename does not already have an extension.  For full details
--   see the
--   <http://msdn.microsoft.com/en-us/library/windows/desktop/aa365527%28v=vs.85%29.aspx documentation>
--   for the Windows @SearchPath@ API.

proc :: FilePath -> [String] -> CreateProcess
proc cmd args = CreateProcess { cmdspec = RawCommand cmd args,
                                cwd = Nothing,
                                env = Nothing,
                                std_in = Inherit,
                                std_out = Inherit,
                                std_err = Inherit,
                                close_fds = False,
                                create_group = False,
                                delegate_ctlc = False}

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
                            delegate_ctlc = False}

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

'createProcess' returns @(/mb_stdin_hdl/, /mb_stdout_hdl/, /mb_stderr_hdl/, /ph/)@,
where

 * if @'std_in' == 'CreatePipe'@, then @/mb_stdin_hdl/@ will be @Just /h/@,
   where @/h/@ is the write end of the pipe connected to the child
   process's @stdin@.

 * otherwise, @/mb_stdin_hdl/ == Nothing@

Similarly for @/mb_stdout_hdl/@ and @/mb_stderr_hdl/@.

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
  r <- createProcess_ "createProcess" cp
  maybeCloseStd (std_in  cp)
  maybeCloseStd (std_out cp)
  maybeCloseStd (std_err cp)
  return r
 where
  maybeCloseStd :: StdStream -> IO ()
  maybeCloseStd (UseHandle hdl)
    | hdl /= stdin && hdl /= stdout && hdl /= stderr = hClose hdl
  maybeCloseStd _ = return ()

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
cleanupProcess (mb_stdin, mb_stdout, mb_stderr, ph) = do
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
    _ <- forkIO (waitForProcess ph >> return ())
    return ()


-- ----------------------------------------------------------------------------
-- spawnProcess/spawnCommand

-- | Creates a new process to run the specified raw command with the given
-- arguments. It does not wait for the program to finish, but returns the
-- 'ProcessHandle'.
--
-- /Since: 1.2.0.0/
spawnProcess :: FilePath -> [String] -> IO ProcessHandle
spawnProcess cmd args = do
    (_,_,_,p) <- createProcess_ "spawnProcess" (proc cmd args)
    return p

-- | Creates a new process to run the specified shell command.
-- It does not wait for the program to finish, but returns the 'ProcessHandle'.
--
-- /Since: 1.2.0.0/
spawnCommand :: String -> IO ProcessHandle
spawnCommand cmd = do
    (_,_,_,p) <- createProcess_ "spawnCommand" (shell cmd)
    return p


-- ----------------------------------------------------------------------------
-- callProcess/callCommand

-- | Creates a new process to run the specified command with the given
-- arguments, and wait for it to finish.  If the command returns a non-zero
-- exit code, an exception is raised.
--
-- If an asynchronous exception is thrown to the thread executing
-- @callProcess@. The forked process will be terminated and
-- @callProcess@ will wait (block) until the process has been
-- terminated.
--
-- /Since: 1.2.0.0/
callProcess :: FilePath -> [String] -> IO ()
callProcess cmd args = do
    exit_code <- withCreateProcess_ "callCommand"
                   (proc cmd args) { delegate_ctlc = True } $ \_ _ _ p ->
                   waitForProcess p
    case exit_code of
      ExitSuccess   -> return ()
      ExitFailure r -> processFailedException "callProcess" cmd args r

-- | Creates a new process to run the specified shell command.  If the
-- command returns a non-zero exit code, an exception is raised.
--
-- If an asynchronous exception is thrown to the thread executing
-- @callCommand@. The forked process will be terminated and
-- @callCommand@ will wait (block) until the process has been
-- terminated.
--
-- /Since: 1.2.0.0/
callCommand :: String -> IO ()
callCommand cmd = do
    exit_code <- withCreateProcess_ "callCommand"
                   (shell cmd) { delegate_ctlc = True } $ \_ _ _ p ->
                   waitForProcess p
    case exit_code of
      ExitSuccess   -> return ()
      ExitFailure r -> processFailedException "callCommand" cmd [] r

processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fun cmd args exit_code =
      ioError (mkIOError OtherError (fun ++ ": " ++ cmd ++
                                     concatMap ((' ':) . show) args ++
                                     " (exit " ++ show exit_code ++ ")")
                                 Nothing Nothing)


-- ----------------------------------------------------------------------------
-- Control-C handling on Unix

-- $ctlc-handling
--
-- When running an interactive console process (such as a shell, console-based
-- text editor or ghci), we typically want that process to be allowed to handle
-- Ctl-C keyboard interrupts how it sees fit. For example, while most programs
-- simply quit on a Ctl-C, some handle it specially. To allow this to happen,
-- use the @'delegate_ctlc' = True@ option in the 'CreateProcess' options.
--
-- The gory details:
--
-- By default Ctl-C will generate a @SIGINT@ signal, causing a 'UserInterrupt'
-- exception to be sent to the main Haskell thread of your program, which if
-- not specially handled will terminate the program. Normally, this is exactly
-- what is wanted: an orderly shutdown of the program in response to Ctl-C.
--
-- Of course when running another interactive program in the console then we
-- want to let that program handle Ctl-C. Under Unix however, Ctl-C sends
-- @SIGINT@ to every process using the console. The standard solution is that
-- while running an interactive program, ignore @SIGINT@ in the parent, and let
-- it be handled in the child process. If that process then terminates due to
-- the @SIGINT@ signal, then at that point treat it as if we had recieved the
-- @SIGINT@ ourselves and begin an orderly shutdown.
--
-- This behaviour is implemented by 'createProcess' (and
-- 'waitForProcess' \/ 'getProcessExitCode') when the @'delegate_ctlc' = True@
-- option is set. In particular, the @SIGINT@ signal will be ignored until
-- 'waitForProcess' returns (or 'getProcessExitCode' returns a non-Nothing
-- result), so it becomes especially important to use 'waitForProcess' for every
-- processes created.
--
-- In addition, in 'delegate_ctlc' mode, 'waitForProcess' and
-- 'getProcessExitCode' will throw a 'UserInterrupt' exception if the process
-- terminated with @'ExitFailure' (-SIGINT)@. Typically you will not want to
-- catch this exception, but let it propagate, giving a normal orderly shutdown.
-- One detail to be aware of is that the 'UserInterrupt' exception is thrown
-- /synchronously/ in the thread that calls 'waitForProcess', whereas normally
-- @SIGINT@ causes the exception to be thrown /asynchronously/ to the main
-- thread.
--
-- For even more detail on this topic, see
-- <http://www.cons.org/cracauer/sigint.html "Proper handling of SIGINT/SIGQUIT">.

-- -----------------------------------------------------------------------------

-- | @readProcess@ forks an external process, reads its standard output
-- strictly, blocking until the process terminates, and returns the output
-- string.
--
-- If an asynchronous exception is thrown to the thread executing
-- @readProcess@. The forked process will be terminated and @readProcess@ will
-- wait (block) until the process has been terminated.
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
-- * A string to pass on standard input to the forked process.
--
readProcess
    :: FilePath                 -- ^ Filename of the executable (see 'proc' for details)
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readProcess cmd args input = do
    let cp_opts = (proc cmd args) {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = Inherit
                  }
    (ex, output) <- withCreateProcess_ "readProcess" cp_opts $
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
     ExitFailure r -> processFailedException "readProcess" cmd args r

{- |
@readProcessWithExitCode@ creates an external process, reads its
standard output and standard error strictly, waits until the process
terminates, and then returns the 'ExitCode' of the process,
the standard output, and the standard error.

If an asynchronous exception is thrown to the thread executing
@readProcessWithExitCode@. The forked process will be terminated and
@readProcessWithExitCode@ will wait (block) until the process has been
terminated.

'readProcess' and 'readProcessWithExitCode' are fairly simple wrappers
around 'createProcess'.  Constructing variants of these functions is
quite easy: follow the link to the source code to see how
'readProcess' is implemented.

On Unix systems, see 'waitForProcess' for the meaning of exit codes
when the process died as the result of a signal.
-}

readProcessWithExitCode
    :: FilePath                 -- ^ Filename of the executable (see 'proc' for details)
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = do
    let cp_opts = (proc cmd args) {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = CreatePipe
                  }
    withCreateProcess_ "readProcessWithExitCode" cp_opts $
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
#if defined(__GLASGOW_HASKELL__)
ignoreSigPipe = C.handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e
#else
ignoreSigPipe = id
#endif

-- ----------------------------------------------------------------------------
-- showCommandForUser

-- | Given a program @/p/@ and arguments @/args/@,
--   @showCommandForUser /p/ /args/@ returns a string suitable for pasting
--   into @\/bin\/sh@ (on Unix systems) or @CMD.EXE@ (on Windows).
showCommandForUser :: FilePath -> [String] -> String
showCommandForUser cmd args = unwords (map translate (cmd : args))


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
    withProcessHandle ph $ \p_ -> do
        case p_ of
            ClosedHandle _ -> return ()
            OpenHandle h -> do
#if mingw32_HOST_OS
                pid <- getProcessId h
                generateConsoleCtrlEvent cTRL_BREAK_EVENT pid
-- We can't use an #elif here, because MIN_VERSION_unix isn't defined
-- on Windows, so on Windows cpp fails:
-- error: missing binary operator before token "("
#else
                pgid <- getProcessGroupIDOf h
                signalProcessGroup sigINT pgid
#endif
                return ()


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


-- ----------------------------------------------------------------------------
-- Old deprecated variants
-- ----------------------------------------------------------------------------

-- TODO: We're not going to mark these functions as DEPRECATED immediately in
-- process-1.2.0.0. That's because some of their replacements have not been
-- around for all that long. But they should eventually be marked with a
-- suitable DEPRECATED pragma after a release or two.


-- ----------------------------------------------------------------------------
-- runCommand

--TODO: in a later release {-# DEPRECATED runCommand "Use 'spawnCommand' instead" #-}

{- | Runs a command using the shell.
 -}
runCommand
  :: String
  -> IO ProcessHandle

runCommand string = do
  (_,_,_,ph) <- createProcess_ "runCommand" (shell string)
  return ph


-- ----------------------------------------------------------------------------
-- runProcess

--TODO: in a later release {-# DEPRECATED runProcess "Use 'spawnProcess' or 'createProcess' instead" #-}

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
  :: FilePath                   -- ^ Filename of the executable (see 'proc' for details)
  -> [String]                   -- ^ Arguments to pass to the executable
  -> Maybe FilePath             -- ^ Optional path to the working directory
  -> Maybe [(String,String)]    -- ^ Optional environment (otherwise inherit)
  -> Maybe Handle               -- ^ Handle to use for @stdin@ (Nothing => use existing @stdin@)
  -> Maybe Handle               -- ^ Handle to use for @stdout@ (Nothing => use existing @stdout@)
  -> Maybe Handle               -- ^ Handle to use for @stderr@ (Nothing => use existing @stderr@)
  -> IO ProcessHandle

runProcess cmd args mb_cwd mb_env mb_stdin mb_stdout mb_stderr = do
  (_,_,_,ph) <-
      createProcess_ "runProcess"
         (proc cmd args){ cwd = mb_cwd,
                          env = mb_env,
                          std_in  = mbToStd mb_stdin,
                          std_out = mbToStd mb_stdout,
                          std_err = mbToStd mb_stderr }
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
-- runInteractiveCommand

--TODO: in a later release {-# DEPRECATED runInteractiveCommand "Use 'createProcess' instead" #-}

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

--TODO: in a later release {-# DEPRECATED runInteractiveCommand "Use 'createProcess' instead" #-}

{- | Runs a raw command, and returns 'Handle's that may be used to communicate
     with the process via its @stdin@, @stdout@ and @stderr@ respectively.

    For example, to start a process and feed a string to its stdin:

>   (inp,out,err,pid) <- runInteractiveProcess "..."
>   forkIO (hPutStr inp str)

    The 'Handle's are initially in binary mode; if you need them to be
    in text mode then use 'hSetBinaryMode'.
-}
runInteractiveProcess
  :: FilePath                   -- ^ Filename of the executable (see 'proc' for details)
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
      createProcess_ fun
           cmd{ std_in  = CreatePipe,
                std_out = CreatePipe,
                std_err = CreatePipe }
  return (fromJust mb_in, fromJust mb_out, fromJust mb_err, p)


-- ---------------------------------------------------------------------------
-- system & rawSystem

--TODO: in a later release {-# DEPRECATED system "Use 'callCommand' (or 'spawnCommand' and 'waitForProcess') instead" #-}

{-|
Computation @system cmd@ returns the exit code produced when the
operating system runs the shell command @cmd@.

This computation may fail with one of the following
'System.IO.Error.IOErrorType' exceptions:

[@PermissionDenied@]
The process has insufficient privileges to perform the operation.

[@ResourceExhausted@]
Insufficient resources are available to perform the operation.

[@UnsupportedOperation@]
The implementation does not support system calls.

On Windows, 'system' passes the command to the Windows command
interpreter (@CMD.EXE@ or @COMMAND.COM@), hence Unixy shell tricks
will not work.

On Unix systems, see 'waitForProcess' for the meaning of exit codes
when the process died as the result of a signal.
-}
#ifdef __GLASGOW_HASKELL__
system :: String -> IO ExitCode
system "" = ioException (ioeSetErrorString (mkIOError InvalidArgument "system" Nothing Nothing) "null command")
system str = do
  (_,_,_,p) <- createProcess_ "system" (shell str) { delegate_ctlc = True }
  waitForProcess p
#endif  /* __GLASGOW_HASKELL__ */


--TODO: in a later release {-# DEPRECATED rawSystem "Use 'callProcess' (or 'spawnProcess' and 'waitForProcess') instead" #-}

{-|
The computation @'rawSystem' /cmd/ /args/@ runs the operating system command
@/cmd/@ in such a way that it receives as arguments the @/args/@ strings
exactly as given, with no funny escaping or shell meta-syntax expansion.
It will therefore behave more portably between operating systems than 'system'.

The return codes and possible failures are the same as for 'system'.
-}
rawSystem :: String -> [String] -> IO ExitCode
#ifdef __GLASGOW_HASKELL__
rawSystem cmd args = do
  (_,_,_,p) <- createProcess_ "rawSystem" (proc cmd args) { delegate_ctlc = True }
  waitForProcess p
#elif !mingw32_HOST_OS
-- crude fallback implementation: could do much better than this under Unix
rawSystem cmd args = system (showCommandForUser cmd args)
#else
rawSystem cmd args = system (showCommandForUser cmd args)
#endif
