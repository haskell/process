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

module System.Process (
    -- * Running sub-processes
    createProcess,
    createProcess_,
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
    readCreateProcess,
    readProcess,
    readCreateProcessWithExitCode,
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

    -- Interprocess communication
    createPipe,

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

import Control.DeepSeq (rnf)
import qualified Control.Exception as C
import Control.Monad
import Data.Maybe
import System.Exit      ( ExitCode(..) )
import System.IO
import System.IO.Error (mkIOError, ioeSetErrorString)

#if defined(mingw32_HOST_OS)
# include <io.h>        /* for _close and _pipe */
# include <fcntl.h>     /* for _O_BINARY */
import Control.Exception (onException)
#else
import System.Posix.Process (getProcessGroupIDOf)
import qualified System.Posix.IO as Posix
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.IO.Exception ( ioException, IOErrorType(..) )
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

Note that @Handle@s provided for @std_in@, @std_out@, or @std_err@ via the
@UseHandle@ constructor will be closed by calling this function. This is not
always the desired behavior. In cases where you would like to leave the
@Handle@ open after spawning the child process, please use 'createProcess_'
instead.

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
    exit_code <- withCreateProcess_ "callProcess"
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
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
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
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readCreateProcess cp input = do
    let cp_opts = cp {
                    std_in  = CreatePipe,
                    std_out = CreatePipe
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
    :: FilePath                 -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
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
    -> String                      -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode cp input = do
    let cp_opts = cp {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = CreatePipe
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

-- ----------------------------------------------------------------------------
-- showCommandForUser

-- | Given a program @/p/@ and arguments @/args/@,
--   @showCommandForUser /p/ /args/@ returns a string suitable for pasting
--   into @\/bin\/sh@ (on Unix systems) or @CMD.EXE@ (on Windows).
showCommandForUser :: FilePath -> [String] -> String
showCommandForUser cmd args = unwords (map translate (cmd : args))


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
  :: FilePath                   -- ^ Filename of the executable (see 'RawCommand' for details)
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
  :: FilePath                   -- ^ Filename of the executable (see 'RawCommand' for details)
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

-- ---------------------------------------------------------------------------
-- createPipe

-- | Create a pipe for interprocess communication and return a
-- @(readEnd, writeEnd)@ `Handle` pair.
--
-- /Since: 1.2.1.0/
createPipe :: IO (Handle, Handle)
#if !mingw32_HOST_OS
createPipe = do
    (readfd, writefd) <- Posix.createPipe
    readh <- Posix.fdToHandle readfd
    writeh <- Posix.fdToHandle writefd
    return (readh, writeh)
#else
createPipe = do
    (readfd, writefd) <- allocaArray 2 $ \ pfds -> do
        throwErrnoIfMinus1_ "_pipe" $ c__pipe pfds 2 (#const _O_BINARY)
        readfd <- peek pfds
        writefd <- peekElemOff pfds 1
        return (readfd, writefd)
    (do readh <- fdToHandle readfd
        writeh <- fdToHandle writefd
        return (readh, writeh)) `onException` (close readfd >> close writefd)

close :: CInt -> IO ()
close = throwErrnoIfMinus1_ "_close" . c__close

foreign import ccall "io.h _pipe" c__pipe ::
    Ptr CInt -> CUInt -> CInt -> IO CInt

foreign import ccall "io.h _close" c__close ::
    CInt -> IO CInt
#endif
