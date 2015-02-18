{-# LANGUAGE CPP, RecordWildCards, BangPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE InterruptibleFFI #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.Internals
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- __Note:__ This module exports internal implementation details that may
-- change anytime.  If you want a more stable API, use "System.Process"
-- instead.
--
-----------------------------------------------------------------------------

module System.Process.Internals (
    ProcessHandle(..), ProcessHandle__(..),
    PHANDLE, closePHANDLE, mkProcessHandle,
    modifyProcessHandle, withProcessHandle,
#ifdef __GLASGOW_HASKELL__
    CreateProcess(..),
    CmdSpec(..), StdStream(..),
    createProcess_,
    runGenProcess_, --deprecated
#endif
    startDelegateControlC,
    endDelegateControlC,
    stopDelegateControlC,
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    pPrPr_disableITimers, c_execvpe,
    ignoreSignal, defaultSignal,
#endif
    withFilePathException, withCEnvironment,
    translate,
    fdToHandle,
    ) where

import Control.Concurrent
import Control.Exception
import Data.Bits
import Data.String
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Control.Monad
import Data.Char
import System.IO
import System.Posix.Process.Internals ( pPrPr_disableITimers, c_execvpe )
import System.Posix.Types
#endif

#ifdef __GLASGOW_HASKELL__
import System.Posix.Internals
import GHC.IO.Exception
import GHC.IO.Encoding
import qualified GHC.IO.FD as FD
import GHC.IO.Device
import GHC.IO.Handle.FD
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types hiding (ClosedHandle)
import System.IO.Error
import Data.Typeable
# if defined(mingw32_HOST_OS)
import GHC.IO.IOMode
import System.Win32.DebugApi (PHANDLE)
# else
import System.Posix.Signals as Sig
# endif
#endif

#if defined(mingw32_HOST_OS)
import System.Directory         ( doesFileExist )
import System.Environment       ( getEnv )
import System.FilePath
#endif

#include "HsProcessConfig.h"
#include "processFlags.h"

#ifdef mingw32_HOST_OS
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

-- ----------------------------------------------------------------------------
-- ProcessHandle type

{- | A handle to a process, which can be used to wait for termination
     of the process using 'System.Process.waitForProcess'.

     None of the process-creation functions in this library wait for
     termination: they all return a 'ProcessHandle' which may be used
     to wait for the process later.
-}
data ProcessHandle__ = OpenHandle PHANDLE | ClosedHandle ExitCode
data ProcessHandle = ProcessHandle !(MVar ProcessHandle__) !Bool

modifyProcessHandle
        :: ProcessHandle
        -> (ProcessHandle__ -> IO (ProcessHandle__, a))
        -> IO a
modifyProcessHandle (ProcessHandle m _) io = modifyMVar m io

withProcessHandle
        :: ProcessHandle
        -> (ProcessHandle__ -> IO a)
        -> IO a
withProcessHandle (ProcessHandle m _) io = withMVar m io

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

type PHANDLE = CPid

mkProcessHandle :: PHANDLE -> Bool -> IO ProcessHandle
mkProcessHandle p mb_delegate_ctlc = do
  m <- newMVar (OpenHandle p)
  return (ProcessHandle m mb_delegate_ctlc)

closePHANDLE :: PHANDLE -> IO ()
closePHANDLE _ = return ()

#else

throwErrnoIfBadPHandle :: String -> IO PHANDLE -> IO PHANDLE
throwErrnoIfBadPHandle = throwErrnoIfNull

-- On Windows, we have to close this HANDLE when it is no longer required,
-- hence we add a finalizer to it
mkProcessHandle :: PHANDLE -> IO ProcessHandle
mkProcessHandle h = do
   m <- newMVar (OpenHandle h)
   _ <- mkWeakMVar m (processHandleFinaliser m)
   return (ProcessHandle m False)

processHandleFinaliser :: MVar ProcessHandle__ -> IO ()
processHandleFinaliser m =
   modifyMVar_ m $ \p_ -> do
        case p_ of
          OpenHandle ph -> closePHANDLE ph
          _ -> return ()
        return (error "closed process handle")

closePHANDLE :: PHANDLE -> IO ()
closePHANDLE ph = c_CloseHandle ph

foreign import WINDOWS_CCONV unsafe "CloseHandle"
  c_CloseHandle
        :: PHANDLE
        -> IO ()
#endif

-- ----------------------------------------------------------------------------

data CreateProcess = CreateProcess{
  cmdspec      :: CmdSpec,                 -- ^ Executable & arguments, or shell command
  cwd          :: Maybe FilePath,          -- ^ Optional path to the working directory for the new process
  env          :: Maybe [(String,String)], -- ^ Optional environment (otherwise inherit from the current process)
  std_in       :: StdStream,               -- ^ How to determine stdin
  std_out      :: StdStream,               -- ^ How to determine stdout
  std_err      :: StdStream,               -- ^ How to determine stderr
  close_fds    :: Bool,                    -- ^ Close all file descriptors except stdin, stdout and stderr in the new process (on Windows, only works if std_in, std_out, and std_err are all Inherit)
  create_group :: Bool,                    -- ^ Create a new process group
  delegate_ctlc:: Bool                     -- ^ Delegate control-C handling. Use this for interactive console processes to let them handle control-C themselves (see below for details).
                                           --
                                           --   On Windows this has no effect.
                                           --
                                           --   /Since: 1.2.0.0/
 }

data CmdSpec
  = ShellCommand String
      -- ^ A command line to execute using the shell
  | RawCommand FilePath [String]
      -- ^ The name of an executable with a list of arguments
      --
      -- The 'FilePath' argument names the executable, and is interpreted
      -- according to the platform's standard policy for searching for
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


-- | construct a `ShellCommand` from a string literal
--
-- /Since: 1.2.1.0/
instance IsString CmdSpec where
  fromString = ShellCommand

data StdStream
  = Inherit                  -- ^ Inherit Handle from parent
  | UseHandle Handle         -- ^ Use the supplied Handle
  | CreatePipe               -- ^ Create a new pipe.  The returned
                             -- @Handle@ will use the default encoding
                             -- and newline translation mode (just
                             -- like @Handle@s created by @openFile@).

-- | This function is almost identical to
-- 'System.Process.createProcess'. The only differences are:
--
-- * 'Handle's provided via 'UseHandle' are not closed automatically.
--
-- * This function takes an extra @String@ argument to be used in creating
--   error messages.
--
-- This function has been available from the "System.Process.Internals" module
-- for some time, and is part of the "System.Process" module since version
-- 1.2.1.0.
--
-- /Since: 1.2.1.0/
createProcess_
  :: String                     -- ^ function name (for error messages)
  -> CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

#ifdef __GLASGOW_HASKELL__

-- -----------------------------------------------------------------------------
-- POSIX runProcess with signal handling in the child

createProcess_ fun CreateProcess{ cmdspec = cmdsp,
                                  cwd = mb_cwd,
                                  env = mb_env,
                                  std_in = mb_stdin,
                                  std_out = mb_stdout,
                                  std_err = mb_stderr,
                                  close_fds = mb_close_fds,
                                  create_group = mb_create_group,
                                  delegate_ctlc = mb_delegate_ctlc }
 = do
  let (cmd,args) = commandToProcess cmdsp
  withFilePathException cmd $
   alloca $ \ pfdStdInput  ->
   alloca $ \ pfdStdOutput ->
   alloca $ \ pfdStdError  ->
   alloca $ \ pFailedDoing ->
   maybeWith withCEnvironment mb_env $ \pEnv ->
   maybeWith withFilePath mb_cwd $ \pWorkDir ->
   withMany withFilePath (cmd:args) $ \cstrs ->
   withArray0 nullPtr cstrs $ \pargs -> do

     fdin  <- mbFd fun fd_stdin  mb_stdin
     fdout <- mbFd fun fd_stdout mb_stdout
     fderr <- mbFd fun fd_stderr mb_stderr

     when mb_delegate_ctlc
       startDelegateControlC

     -- runInteractiveProcess() blocks signals around the fork().
     -- Since blocking/unblocking of signals is a global state
     -- operation, we better ensure mutual exclusion of calls to
     -- runInteractiveProcess().
     proc_handle <- withMVar runInteractiveProcess_lock $ \_ ->
                         c_runInteractiveProcess pargs pWorkDir pEnv
                                fdin fdout fderr
                                pfdStdInput pfdStdOutput pfdStdError
                                (if mb_delegate_ctlc then 1 else 0)
                                ((if mb_close_fds then RUN_PROCESS_IN_CLOSE_FDS else 0)
                                .|.(if mb_create_group then RUN_PROCESS_IN_NEW_GROUP else 0))
                                pFailedDoing

     when (proc_handle == -1) $ do
         cFailedDoing <- peek pFailedDoing
         failedDoing <- peekCString cFailedDoing
         when mb_delegate_ctlc
           stopDelegateControlC
         throwErrno (fun ++ ": " ++ failedDoing)

     hndStdInput  <- mbPipe mb_stdin  pfdStdInput  WriteMode
     hndStdOutput <- mbPipe mb_stdout pfdStdOutput ReadMode
     hndStdError  <- mbPipe mb_stderr pfdStdError  ReadMode

     ph <- mkProcessHandle proc_handle mb_delegate_ctlc
     return (hndStdInput, hndStdOutput, hndStdError, ph)

{-# NOINLINE runInteractiveProcess_lock #-}
runInteractiveProcess_lock :: MVar ()
runInteractiveProcess_lock = unsafePerformIO $ newMVar ()

-- ----------------------------------------------------------------------------
-- Delegated control-C handling on Unix

-- See ticket https://ghc.haskell.org/trac/ghc/ticket/2301
-- and http://www.cons.org/cracauer/sigint.html
--
-- While running an interactive console process like ghci or a shell, we want
-- to let that process handle Ctl-C keyboard interrupts how it sees fit.
-- So that means we need to ignore the SIGINT/SIGQUIT Unix signals while we're
-- running such programs. And then if/when they do terminate, we need to check
-- if they terminated due to SIGINT/SIGQUIT and if so then we behave as if we
-- got the Ctl-C then, by throwing the UserInterrupt exception.
--
-- If we run multiple programs like this concurrently then we have to be
-- careful to avoid messing up the signal handlers. We keep a count and only
-- restore when the last one has finished.

{-# NOINLINE runInteractiveProcess_delegate_ctlc #-}
runInteractiveProcess_delegate_ctlc :: MVar (Maybe (Int, Sig.Handler, Sig.Handler))
runInteractiveProcess_delegate_ctlc = unsafePerformIO $ newMVar Nothing

startDelegateControlC :: IO ()
startDelegateControlC =
    modifyMVar_ runInteractiveProcess_delegate_ctlc $ \delegating -> do
      case delegating of
        Nothing -> do
          -- We're going to ignore ^C in the parent while there are any
          -- processes using ^C delegation.
          --
          -- If another thread runs another process without using
          -- delegation while we're doing this then it will inherit the
          -- ignore ^C status.
          old_int  <- installHandler sigINT  Ignore Nothing
          old_quit <- installHandler sigQUIT Ignore Nothing
          return (Just (1, old_int, old_quit))

        Just (count, old_int, old_quit) -> do
          -- If we're already doing it, just increment the count
          let !count' = count + 1
          return (Just (count', old_int, old_quit))

stopDelegateControlC :: IO ()
stopDelegateControlC =
    modifyMVar_ runInteractiveProcess_delegate_ctlc $ \delegating -> do
      case delegating of
        Just (1, old_int, old_quit) -> do
          -- Last process, so restore the old signal handlers
          _ <- installHandler sigINT  old_int  Nothing
          _ <- installHandler sigQUIT old_quit Nothing
          return Nothing

        Just (count, old_int, old_quit) -> do
          -- Not the last, just decrement the count
          let !count' = count - 1
          return (Just (count', old_int, old_quit))

        Nothing -> return Nothing -- should be impossible

endDelegateControlC :: ExitCode -> IO ()
endDelegateControlC exitCode = do
    stopDelegateControlC

    -- And if the process did die due to SIGINT or SIGQUIT then
    -- we throw our equivalent exception here (synchronously).
    --
    -- An alternative design would be to throw to the main thread, as the
    -- normal signal handler does. But since we can be sync here, we do so.
    -- It allows the code locally to catch it and do something.
    case exitCode of
      ExitFailure n | isSigIntQuit n -> throwIO UserInterrupt
      _                              -> return ()
  where
    isSigIntQuit n = sig == sigINT || sig == sigQUIT
      where
        sig = fromIntegral (-n)

foreign import ccall unsafe "runInteractiveProcess"
  c_runInteractiveProcess
        ::  Ptr CString
        -> CString
        -> Ptr CString
        -> FD
        -> FD
        -> FD
        -> Ptr FD
        -> Ptr FD
        -> Ptr FD
        -> CInt                         -- reset child's SIGINT & SIGQUIT handlers
        -> CInt                         -- flags
        -> Ptr CString
        -> IO PHANDLE

#endif /* __GLASGOW_HASKELL__ */

ignoreSignal, defaultSignal :: CLong
ignoreSignal  = CONST_SIG_IGN
defaultSignal = CONST_SIG_DFL

#else

#ifdef __GLASGOW_HASKELL__

createProcess_ fun CreateProcess{ cmdspec = cmdsp,
                                  cwd = mb_cwd,
                                  env = mb_env,
                                  std_in = mb_stdin,
                                  std_out = mb_stdout,
                                  std_err = mb_stderr,
                                  close_fds = mb_close_fds,
                                  create_group = mb_create_group,
                                  delegate_ctlc = _ignored }
 = do
  (cmd, cmdline) <- commandToProcess cmdsp
  withFilePathException cmd $
   alloca $ \ pfdStdInput  ->
   alloca $ \ pfdStdOutput ->
   alloca $ \ pfdStdError  ->
   maybeWith withCEnvironment mb_env $ \pEnv ->
   maybeWith withCWString mb_cwd $ \pWorkDir -> do
   withCWString cmdline $ \pcmdline -> do

     fdin  <- mbFd fun fd_stdin  mb_stdin
     fdout <- mbFd fun fd_stdout mb_stdout
     fderr <- mbFd fun fd_stderr mb_stderr

     -- #2650: we must ensure mutual exclusion of c_runInteractiveProcess,
     -- because otherwise there is a race condition whereby one thread
     -- has created some pipes, and another thread spawns a process which
     -- accidentally inherits some of the pipe handles that the first
     -- thread has created.
     --
     -- An MVar in Haskell is the best way to do this, because there
     -- is no way to do one-time thread-safe initialisation of a mutex
     -- the C code.  Also the MVar will be cheaper when not running
     -- the threaded RTS.
     proc_handle <- withMVar runInteractiveProcess_lock $ \_ ->
                    throwErrnoIfBadPHandle fun $
                         c_runInteractiveProcess pcmdline pWorkDir pEnv
                                fdin fdout fderr
                                pfdStdInput pfdStdOutput pfdStdError
                                ((if mb_close_fds then RUN_PROCESS_IN_CLOSE_FDS else 0)
                                .|.(if mb_create_group then RUN_PROCESS_IN_NEW_GROUP else 0))

     hndStdInput  <- mbPipe mb_stdin  pfdStdInput  WriteMode
     hndStdOutput <- mbPipe mb_stdout pfdStdOutput ReadMode
     hndStdError  <- mbPipe mb_stderr pfdStdError  ReadMode

     ph <- mkProcessHandle proc_handle
     return (hndStdInput, hndStdOutput, hndStdError, ph)

{-# NOINLINE runInteractiveProcess_lock #-}
runInteractiveProcess_lock :: MVar ()
runInteractiveProcess_lock = unsafePerformIO $ newMVar ()

-- The following functions are always present in the export list. For
-- compatibility with the non-Windows code, we provide the same functions with
-- matching type signatures, but implemented as no-ops. For details, see:
-- <https://github.com/haskell/process/pull/21>
startDelegateControlC :: IO ()
startDelegateControlC = return ()

endDelegateControlC :: ExitCode -> IO ()
endDelegateControlC _ = return ()

stopDelegateControlC :: IO ()
stopDelegateControlC = return ()

-- End no-op functions

foreign import ccall unsafe "runInteractiveProcess"
  c_runInteractiveProcess
        :: CWString
        -> CWString
        -> Ptr CWString
        -> FD
        -> FD
        -> FD
        -> Ptr FD
        -> Ptr FD
        -> Ptr FD
        -> CInt                         -- flags
        -> IO PHANDLE
#endif

#endif /* __GLASGOW_HASKELL__ */

fd_stdin, fd_stdout, fd_stderr :: FD
fd_stdin  = 0
fd_stdout = 1
fd_stderr = 2

mbFd :: String -> FD -> StdStream -> IO FD
mbFd _   _std CreatePipe      = return (-1)
mbFd _fun std Inherit         = return std
mbFd fun _std (UseHandle hdl) =
  withHandle fun hdl $ \Handle__{haDevice=dev,..} ->
    case cast dev of
      Just fd -> do
         -- clear the O_NONBLOCK flag on this FD, if it is set, since
         -- we're exposing it externally (see #3316)
         fd' <- FD.setNonBlockingMode fd False
         return (Handle__{haDevice=fd',..}, FD.fdFD fd')
      Nothing ->
          ioError (mkIOError illegalOperationErrorType
                      "createProcess" (Just hdl) Nothing
                   `ioeSetErrorString` "handle is not a file descriptor")

mbPipe :: StdStream -> Ptr FD -> IOMode -> IO (Maybe Handle)
mbPipe CreatePipe pfd  mode = fmap Just (pfdToHandle pfd mode)
mbPipe _std      _pfd _mode = return Nothing

pfdToHandle :: Ptr FD -> IOMode -> IO Handle
pfdToHandle pfd mode = do
  fd <- peek pfd
  let filepath = "fd:" ++ show fd
  (fD,fd_type) <- FD.mkFD (fromIntegral fd) mode
                       (Just (Stream,0,0)) -- avoid calling fstat()
                       False {-is_socket-}
                       False {-non-blocking-}
  fD' <- FD.setNonBlockingMode fD True -- see #3316
#if __GLASGOW_HASKELL__ >= 704
  enc <- getLocaleEncoding
#else
  let enc = localeEncoding
#endif
  mkHandleFromFD fD' fd_type filepath mode False {-is_socket-} (Just enc)

-- ----------------------------------------------------------------------------
-- commandToProcess

{- | Turns a shell command into a raw command.  Usually this involves
     wrapping it in an invocation of the shell.

   There's a difference in the signature of commandToProcess between
   the Windows and Unix versions.  On Unix, exec takes a list of strings,
   and we want to pass our command to /bin/sh as a single argument.

   On Windows, CreateProcess takes a single string for the command,
   which is later decomposed by cmd.exe.  In this case, we just want
   to prepend @\"c:\WINDOWS\CMD.EXE \/c\"@ to our command line.  The
   command-line translation that we normally do for arguments on
   Windows isn't required (or desirable) here.
-}

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

commandToProcess :: CmdSpec -> (FilePath, [String])
commandToProcess (ShellCommand string) = ("/bin/sh", ["-c", string])
commandToProcess (RawCommand cmd args) = (cmd, args)

#else

commandToProcess
  :: CmdSpec
  -> IO (FilePath, String)
commandToProcess (ShellCommand string) = do
  cmd <- findCommandInterpreter
  return (cmd, translate cmd ++ " /c " ++ string)
        -- We don't want to put the cmd into a single
        -- argument, because cmd.exe will not try to split it up.  Instead,
        -- we just tack the command on the end of the cmd.exe command line,
        -- which partly works.  There seem to be some quoting issues, but
        -- I don't have the energy to find+fix them right now (ToDo). --SDM
        -- (later) Now I don't know what the above comment means.  sigh.
commandToProcess (RawCommand cmd args) = do
  return (cmd, translate cmd ++ concatMap ((' ':) . translate) args)

-- Find CMD.EXE (or COMMAND.COM on Win98).  We use the same algorithm as
-- system() in the VC++ CRT (Vc7/crt/src/system.c in a VC++ installation).
findCommandInterpreter :: IO FilePath
findCommandInterpreter = do
  -- try COMSPEC first
  catchJust (\e -> if isDoesNotExistError e then Just e else Nothing)
            (getEnv "COMSPEC") $ \_ -> do

    -- try to find CMD.EXE or COMMAND.COM
    {-
    XXX We used to look at _osver (using cbits) and pick which shell to
    use with
    let filename | osver .&. 0x8000 /= 0 = "command.com"
                 | otherwise             = "cmd.exe"
    We ought to use GetVersionEx instead, but for now we just look for
    either filename
    -}
    path <- getEnv "PATH"
    let
        -- use our own version of System.Directory.findExecutable, because
        -- that assumes the .exe suffix.
        search :: [FilePath] -> IO (Maybe FilePath)
        search [] = return Nothing
        search (d:ds) = do
                let path1 = d </> "cmd.exe"
                    path2 = d </> "command.com"
                b1 <- doesFileExist path1
                b2 <- doesFileExist path2
                if b1 then return (Just path1)
                      else if b2 then return (Just path2)
                                 else search ds
    --
    mb_path <- search (splitSearchPath path)

    case mb_path of
      Nothing -> ioError (mkIOError doesNotExistErrorType
                                "findCommandInterpreter" Nothing Nothing)
      Just cmd -> return cmd
#endif

-- ------------------------------------------------------------------------
-- Escaping commands for shells

{-
On Windows we also use this for running commands.  We use CreateProcess,
passing a single command-line string (lpCommandLine) as its argument.
(CreateProcess is well documented on http://msdn.microsoft.com.)

      - It parses the beginning of the string to find the command. If the
        file name has embedded spaces, it must be quoted, using double
        quotes thus
                "foo\this that\cmd" arg1 arg2

      - The invoked command can in turn access the entire lpCommandLine string,
        and the C runtime does indeed do so, parsing it to generate the
        traditional argument vector argv[0], argv[1], etc.  It does this
        using a complex and arcane set of rules which are described here:

           http://msdn.microsoft.com/library/default.asp?url=/library/en-us/vccelng/htm/progs_12.asp

        (if this URL stops working, you might be able to find it by
        searching for "Parsing C Command-Line Arguments" on MSDN.  Also,
        the code in the Microsoft C runtime that does this translation
        is shipped with VC++).

Our goal in runProcess is to take a command filename and list of
arguments, and construct a string which inverts the translatsions
described above, such that the program at the other end sees exactly
the same arguments in its argv[] that we passed to rawSystem.

This inverse translation is implemented by 'translate' below.

Here are some pages that give informations on Windows-related
limitations and deviations from Unix conventions:

    http://support.microsoft.com/default.aspx?scid=kb;en-us;830473
    Command lines and environment variables effectively limited to 8191
    characters on Win XP, 2047 on NT/2000 (probably even less on Win 9x):

    http://www.microsoft.com/windowsxp/home/using/productdoc/en/default.asp?url=/WINDOWSXP/home/using/productdoc/en/percent.asp
    Command-line substitution under Windows XP. IIRC these facilities (or at
    least a large subset of them) are available on Win NT and 2000. Some
    might be available on Win 9x.

    http://www.microsoft.com/windowsxp/home/using/productdoc/en/default.asp?url=/WINDOWSXP/home/using/productdoc/en/Cmd.asp
    How CMD.EXE processes command lines.


Note: CreateProcess does have a separate argument (lpApplicationName)
with which you can specify the command, but we have to slap the
command into lpCommandLine anyway, so that argv[0] is what a C program
expects (namely the application name).  So it seems simpler to just
use lpCommandLine alone, which CreateProcess supports.
-}

translate :: String -> String
#if mingw32_HOST_OS
translate xs = '"' : snd (foldr escape (True,"\"") xs)
  where escape '"'  (_,     str) = (True,  '\\' : '"'  : str)
        escape '\\' (True,  str) = (True,  '\\' : '\\' : str)
        escape '\\' (False, str) = (False, '\\' : str)
        escape c    (_,     str) = (False, c : str)
        -- See long comment above for what this function is trying to do.
        --
        -- The Bool passed back along the string is True iff the
        -- rest of the string is a sequence of backslashes followed by
        -- a double quote.
#else
translate "" = "''"
translate str
   -- goodChar is a pessimistic predicate, such that if an argument is
   -- non-empty and only contains goodChars, then there is no need to
   -- do any quoting or escaping
 | all goodChar str = str
 | otherwise        = '\'' : foldr escape "'" str
  where escape '\'' = showString "'\\''"
        escape c    = showChar c
        goodChar c = isAlphaNum c || c `elem` "-_.,/"
#endif

-- ----------------------------------------------------------------------------
-- Utils

withFilePathException :: FilePath -> IO a -> IO a
withFilePathException fpath act = handle mapEx act
  where
    mapEx ex = ioError (ioeSetFileName ex fpath)

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
withCEnvironment :: [(String,String)] -> (Ptr CString  -> IO a) -> IO a
withCEnvironment envir act =
  let env' = map (\(name, val) -> name ++ ('=':val)) envir
  in withMany withCString env' (\pEnv -> withArray0 nullPtr pEnv act)
#else
withCEnvironment :: [(String,String)] -> (Ptr CWString -> IO a) -> IO a
withCEnvironment envir act =
  let env' = foldr (\(name, val) env -> name ++ ('=':val)++'\0':env) "\0" envir
  in withCWString env' (act . castPtr)
#endif


-- ----------------------------------------------------------------------------
-- Deprecated / compat

#ifdef __GLASGOW_HASKELL__
{-# DEPRECATED runGenProcess_
      "Please do not use this anymore, use the ordinary 'System.Process.createProcess'. If you need the SIGINT handling, use delegate_ctlc = True (runGenProcess_ is now just an imperfectly emulated stub that probably duplicates or overrides your own signal handling)." #-}
runGenProcess_
 :: String                     -- ^ function name (for error messages)
 -> CreateProcess
 -> Maybe CLong                -- ^ handler for SIGINT
 -> Maybe CLong                -- ^ handler for SIGQUIT
 -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
runGenProcess_ fun c (Just sig) (Just sig') | sig == defaultSignal && sig == sig'
                         = createProcess_ fun c { delegate_ctlc = True }
runGenProcess_ fun c _ _ = createProcess_ fun c
#else
runGenProcess_ fun c _ _ = createProcess_ fun c
#endif

#endif
