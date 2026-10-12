{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

#if defined(OS_STRING)
#define PATH_TYPE OsPath
#define STRING_TYPE OsString
#define CHAR_TYPE OsChar
#else
#define PATH_TYPE FilePath
#define STRING_TYPE String
#define CHAR_TYPE Char
#endif

module System.Process.Windows.STRING_TYPE
    ( mkProcessHandle
    , translateInternal
    , createProcess_Internal
    , withCEnvironment
    , closePHANDLE
    , startDelegateControlC
    , endDelegateControlC
    , stopDelegateControlC
    , isDefaultSignal
    , createPipeInternal
    , createPipeInternalFd
    , interruptProcessGroupOfInternal
    , terminateJob
    , terminateJobUnsafe
    , waitForJobCompletion
    , timeout_Infinite
    , HANDLE
    , mkNamedPipe
#if defined(__IO_MANAGER_WINIO__)
    , rawHANDLEToHandle
#endif
    ) where

import Control.Concurrent (MVar, mkWeakMVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (onException)
import Control.Monad (when)
import Data.Bits ((.|.))
import Data.Char (toLower)
import Foreign
  ( Ptr
  , Storable
  , WordPtr
  , alloca
  , allocaArray
  , allocaBytes
  , castPtr
  , maybeWith
  , nullPtr
  , peek
  , peekElemOff
  , sizeOf
  )
import Foreign.C
  ( CInt(CInt)
  , CUInt(CUInt)
  , CLong
  , CWString
  , throwErrnoIf_
  , throwErrnoIfMinus1_
  , throwErrnoIfNull
  )
import System.IO.Unsafe (unsafePerformIO)

import System.Posix.Internals (FD)
import GHC.IO.Handle.FD (fdToHandle)
import System.Exit (ExitCode)
import System.IO (Handle, IOMode(ReadMode, WriteMode))
import System.IO.Error (doesNotExistErrorType, mkIOError)

import System.Win32.Console (generateConsoleCtrlEvent, cTRL_BREAK_EVENT)
import System.Win32.Process (getProcessId)

import System.Process.Common.STRING_TYPE

-- WinIO imports
#if defined(__IO_MANAGER_WINIO__)
import Foreign (intPtrToPtr)
import GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.Handle.Windows (handleToHANDLE, mkHandleFromHANDLE)
import GHC.IO.SubSystem ((<!>))
import qualified GHC.Event.Windows as Manager
import GHC.IO.Windows.Handle (Io(), NativeHandle(), fromHANDLE)
import Graphics.Win32.Misc
  ( getStdHandle
  , sTD_ERROR_HANDLE
  , sTD_INPUT_HANDLE
  , sTD_OUTPUT_HANDLE
  )
import System.Win32.Types (HANDLE)
#endif

-- OsString imports
#if defined(OS_STRING)
import System.Directory.OsPath (doesFileExist)
import System.OsPath ((</>), splitSearchPath, takeExtension)
import System.Process.Environment.OsString (getEnv)
import System.Process.Windows.OsString.Compat
  ( dropWhileEnd
  , intercalate
  )
import qualified System.Process.Windows.OsString.Compat as OsString

-- String imports
#else
import Data.List (dropWhileEnd, intercalate)
import System.Directory (doesFileExist)
import System.FilePath ((</>), splitSearchPath, takeExtension)
import System.Environment.Blank (getEnv)
#endif

-- Architecture-dependent imports
#if defined(i386_HOST_ARCH)
#define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH) || defined(aarch64_HOST_ARCH)
#define WINDOWS_CCONV ccall
#else
#error Unknown mingw32 arch
#endif

foreign import capi "fcntl.h value _O_BINARY" c_o_binary :: CInt

#include "processFlags.h"

throwErrnoIfBadPHandle :: String -> IO PHANDLE -> IO PHANDLE
throwErrnoIfBadPHandle = throwErrnoIfNull

-- On Windows, we have to close this HANDLE when it is no longer required,
-- hence we add a finalizer to it
mkProcessHandle :: PHANDLE -> Bool -> PHANDLE -> IO ProcessHandle
mkProcessHandle h ignore_signals job = do
   m <- if job == nullPtr
           then newMVar (OpenHandle h)
           else newMVar (OpenExtHandle h job)
   _ <- mkWeakMVar m (processHandleFinaliser m)
   l <- newMVar ()
   return (ProcessHandle m ignore_signals l)

processHandleFinaliser :: MVar ProcessHandle__ -> IO ()
processHandleFinaliser m =
   modifyMVar_ m $ \p_ -> do
        case p_ of
          OpenHandle ph           -> closePHANDLE ph
          OpenExtHandle ph job    -> closePHANDLE ph
                                  >> closePHANDLE job
          _ -> return ()
        return (error "closed process handle")

closePHANDLE :: PHANDLE -> IO ()
closePHANDLE ph = c_CloseHandle ph

foreign import WINDOWS_CCONV unsafe "CloseHandle"
  c_CloseHandle
        :: PHANDLE
        -> IO ()

createProcess_Internal
  :: String                     -- ^ function name (for error messages)
  -> CreateProcess
  -> IO ProcRetHandles

#if defined(__IO_MANAGER_WINIO__)
createProcess_Internal = createProcess_Internal_mio <!> createProcess_Internal_winio
#else
createProcess_Internal = createProcess_Internal_mio
#endif

createProcess_Internal_mio
  :: String                     -- ^ function name (for error messages)
  -> CreateProcess
  -> IO ProcRetHandles

createProcess_Internal_mio fun def@CreateProcess{
    std_in = mb_stdin,
    std_out = mb_stdout,
    std_err = mb_stderr,
    close_fds = mb_close_fds,
    create_group = mb_create_group,
    detach_console = mb_detach_console,
    create_new_console = mb_create_new_console,
    new_session = mb_new_session,
    use_process_jobs = use_job }
 = createProcess_Internal_wrapper fun def $
       \pfdStdInput pfdStdOutput pfdStdError hJob pEnv pWorkDir pcmdline -> do
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
                                  .|.(if mb_create_group then RUN_PROCESS_IN_NEW_GROUP else 0)
                                  .|.(if mb_detach_console then RUN_PROCESS_DETACHED else 0)
                                  .|.(if mb_create_new_console then RUN_PROCESS_NEW_CONSOLE else 0)
                                  .|.(if mb_new_session then RUN_PROCESS_NEW_SESSION else 0))
                                  use_job
                                  hJob

       hndStdInput  <- mbPipe mb_stdin  pfdStdInput  WriteMode
       hndStdOutput <- mbPipe mb_stdout pfdStdOutput ReadMode
       hndStdError  <- mbPipe mb_stderr pfdStdError  ReadMode

       return (proc_handle, hndStdInput, hndStdOutput, hndStdError)


createProcess_Internal_wrapper
  :: Storable a => String                     -- ^ function name (for error messages)
  -> CreateProcess
  -> (Ptr a -> Ptr a -> Ptr a -> Ptr PHANDLE -> Ptr CWString -> CWString
      -> CWString -> IO (PHANDLE, Maybe Handle, Maybe Handle, Maybe Handle))
  -> IO ProcRetHandles

createProcess_Internal_wrapper _fun CreateProcess{
    cmdspec = cmdsp,
    cwd = mb_cwd,
    env = mb_env,
    delegate_ctlc = ignore_signals }
    action
 = do
  let lenPtr = sizeOf (undefined :: WordPtr)
  (cmd, cmdline) <- commandToProcess cmdsp
  withFilePathException cmd $
   alloca $ \ pfdStdInput           ->
   alloca $ \ pfdStdOutput          ->
   alloca $ \ pfdStdError           ->
   allocaBytes lenPtr $ \ hJob      ->
   maybeWith withCEnvironment mb_env $ \pEnv ->
   maybeWith useAsForeignString mb_cwd $ \pWorkDir -> do
   useAsForeignString cmdline $ \pcmdline -> do

     (proc_handle, hndStdInput, hndStdOutput, hndStdError)
       <- action pfdStdInput pfdStdOutput pfdStdError hJob pEnv pWorkDir pcmdline

     -- If we have successfully created the process then check if we have to
     -- detach from the console.  I'm not sure why the posix version changes
     -- the state right before creating the child process, but doing so here
     -- means the first child also inherits this
     when ignore_signals $
       startDelegateControlC

     phJob  <- peek hJob
     ph     <- mkProcessHandle proc_handle ignore_signals phJob
     return ProcRetHandles { hStdInput  = hndStdInput
                           , hStdOutput = hndStdOutput
                           , hStdError  = hndStdError
                           , procHandle = ph
                           }

#if defined(__IO_MANAGER_WINIO__)
createProcess_Internal_winio
  :: String                     -- ^ function name (for error messages)
  -> CreateProcess
  -> IO ProcRetHandles

createProcess_Internal_winio fun def@CreateProcess{
    std_in = mb_stdin,
    std_out = mb_stdout,
    std_err = mb_stderr,
    close_fds = mb_close_fds,
    create_group = mb_create_group,
    detach_console = mb_detach_console,
    create_new_console = mb_create_new_console,
    new_session = mb_new_session,
    use_process_jobs = use_job }
 = createProcess_Internal_wrapper fun def $
       \pfdStdInput pfdStdOutput pfdStdError hJob pEnv pWorkDir pcmdline -> do

     _stdin  <- getStdHandle sTD_INPUT_HANDLE
     _stdout <- getStdHandle sTD_OUTPUT_HANDLE
     _stderr <- getStdHandle sTD_ERROR_HANDLE
     hwnd_in  <- mbHANDLE _stdin  mb_stdin
     hwnd_out <- mbHANDLE _stdout mb_stdout
     hwnd_err <- mbHANDLE _stderr mb_stderr

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
                         c_runInteractiveProcessHANDLE pcmdline pWorkDir pEnv
                                hwnd_in hwnd_out hwnd_err
                                pfdStdInput pfdStdOutput pfdStdError
                                ((if mb_close_fds then RUN_PROCESS_IN_CLOSE_FDS else 0)
                                .|.(if mb_create_group then RUN_PROCESS_IN_NEW_GROUP else 0)
                                .|.(if mb_detach_console then RUN_PROCESS_DETACHED else 0)
                                .|.(if mb_create_new_console then RUN_PROCESS_NEW_CONSOLE else 0)
                                .|.(if mb_new_session then RUN_PROCESS_NEW_SESSION else 0))
                                use_job
                                hJob

     -- Attach the handle to the I/O manager's CompletionPort.  This allows the
     -- I/O manager to service requests for this Handle.
     Manager.associateHandle' =<< peek pfdStdInput
     Manager.associateHandle' =<< peek pfdStdOutput
     Manager.associateHandle' =<< peek pfdStdError

     -- Create the haskell mode handles as files.
     hndStdInput  <- mbPipeHANDLE mb_stdin  pfdStdInput  WriteMode
     hndStdOutput <- mbPipeHANDLE mb_stdout pfdStdOutput ReadMode
     hndStdError  <- mbPipeHANDLE mb_stderr pfdStdError  ReadMode

     return (proc_handle, hndStdInput, hndStdOutput, hndStdError)

#endif

{-# NOINLINE runInteractiveProcess_lock #-}
runInteractiveProcess_lock :: MVar ()
runInteractiveProcess_lock = unsafePerformIO $ newMVar ()

-- ----------------------------------------------------------------------------
-- Delegated control-C handling on Windows

-- See https://learn.microsoft.com/en-us/windows/console/setconsolectrlhandler
--
-- While running an interactive console process like ghci or a shell, we want
-- to let that process handle Ctl-C keyboard interrupts how it sees fit.
-- So that means we need to ignore the CTRL_C_EVENT/CTRL_BREAK_EVENT Windows
-- events while we're running such programs.
--
-- If we run multiple programs like this concurrently then we have to be
-- careful to avoid messing up the signal handlers. We keep a count and only
-- restore when the last one has finished.
--
-- To do this we have to use SetConsoleCtrlHandler which masks the events for
-- the current process and any child it creates from that point.
--
-- In this case we can't use FreeConsole/AttachConsole since those destroy
-- the signal handler stack for the application when called.  This means we'd
-- have to recreate them and process doesn't know what's there.

{-# NOINLINE runInteractiveProcess_delegate_ctlc #-}
runInteractiveProcess_delegate_ctlc :: MVar (Maybe (Int))
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
          _ <- c_setConsoleCtrlHandler nullPtr True
          return (Just 1)

        Just count -> do
          -- If we're already doing it, just increment the count
          let !count' = count + 1
          return (Just count')

stopDelegateControlC :: IO ()
stopDelegateControlC =
    modifyMVar_ runInteractiveProcess_delegate_ctlc $ \delegating -> do
      case delegating of
        Just 1 -> do
          -- Last process, so restore the old signal handlers
          _ <- c_setConsoleCtrlHandler nullPtr False
          return Nothing

        Just count -> do
          -- Not the last, just decrement the count
          let !count' = count - 1
          return (Just count')

        Nothing -> return Nothing -- should be impossible

endDelegateControlC :: ExitCode -> IO ()
-- I don't think there's a standard exit code for program interruptions
-- on Windows, so I'll just ignore it for now.
endDelegateControlC _ = stopDelegateControlC


-- End no-op functions


-- ----------------------------------------------------------------------------
-- Interface to C I/O CP bits

-- | Variant of terminateJob that is not thread-safe
terminateJobUnsafe :: ProcessHandle__ -> CUInt -> IO Bool
terminateJobUnsafe p_  ecode = do
        case p_ of
            ClosedHandle      _ -> return False
            OpenHandle        _ -> return False
            OpenExtHandle _ job -> c_terminateJobObject job ecode

terminateJob :: ProcessHandle -> CUInt -> IO Bool
terminateJob jh ecode =
    withProcessHandle jh $ \p_ -> terminateJobUnsafe p_ ecode

timeout_Infinite :: CUInt
timeout_Infinite = 0xFFFFFFFF

waitForJobCompletion :: PHANDLE -- ^ job handle
                     -> IO ()
waitForJobCompletion job =
    throwErrnoIf_ not "waitForJobCompletion" $ c_waitForJobCompletion job

-- ----------------------------------------------------------------------------
-- Interface to C bits

foreign import WINDOWS_CCONV unsafe "TerminateJobObject"
  c_terminateJobObject
        :: PHANDLE
        -> CUInt
        -> IO Bool

foreign import WINDOWS_CCONV unsafe "SetConsoleCtrlHandler"
  c_setConsoleCtrlHandler
        :: Ptr ()
        -> Bool
        -> IO Bool

foreign import ccall interruptible "waitForJobCompletion" -- NB. safe - can block
  c_waitForJobCompletion
        :: PHANDLE
        -> IO Bool

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
        -> CInt          -- flags
        -> Bool          -- useJobObject
        -> Ptr PHANDLE       -- Handle to Job
        -> IO PHANDLE

#if defined(__IO_MANAGER_WINIO__)
foreign import ccall unsafe "runInteractiveProcessHANDLE"
  c_runInteractiveProcessHANDLE
        :: CWString
        -> CWString
        -> Ptr CWString
        -> HANDLE
        -> HANDLE
        -> HANDLE
        -> Ptr HANDLE
        -> Ptr HANDLE
        -> Ptr HANDLE
        -> CInt          -- flags
        -> Bool          -- useJobObject
        -> Ptr PHANDLE       -- Handle to Job
        -> IO PHANDLE
#endif

commandToProcess :: CmdSpec -> IO (PATH_TYPE, STRING_TYPE)
commandToProcess (ShellCommand string) = do
  cmd <- findCommandInterpreter
  return (cmd, translateInternal cmd <> slashC <> string)
        -- We don't want to put the cmd into a single
        -- argument, because cmd.exe will not try to split it up.  Instead,
        -- we just tack the command on the end of the cmd.exe command line,
        -- which partly works.  There seem to be some quoting issues, but
        -- I don't have the energy to find+fix them right now (ToDo). --SDM
        -- (later) Now I don't know what the above comment means.  sigh.
  where
    slashC :: STRING_TYPE
#if defined(OS_STRING)
    slashC = [osstr| /c |]
#else
    slashC = " /c "
#endif
commandToProcess (RawCommand cmd args) = do
  let
    cmdTranslated = translateInternal cmd
    argsTranslated = map translateArg args
    translated = cmdTranslated : argsTranslated
  return (cmd, intercalate separator translated)
  where
    translateArg :: STRING_TYPE -> STRING_TYPE
    translateArg
      | map toLower (unsafeDecodeFS (takeWinExtension cmd)) `elem` [".bat", ".cmd"] =
        translateCmdExeArg
      | otherwise = translateInternal

    separator :: STRING_TYPE
#if defined(OS_STRING)
    separator = [osstr| |]
#else
    separator = " "
#endif

-- TODO: filepath should also be updated with 'takeWinExtension'. Perhaps
-- some day we can remove this logic from `process` but there is no hurry.

-- | Get the extension of a Windows file, removing any trailing spaces or dots
-- since they are ignored.
--
-- See: <https://learn.microsoft.com/en-us/troubleshoot/windows-client/shell-experience/file-folder-name-whitespace-characters>
--
-- >>> takeWinExtension "test.bat."
-- ".bat"
--
-- >>> takeWinExtension "test.bat ."
-- ".bat"
takeWinExtension :: PATH_TYPE -> STRING_TYPE
takeWinExtension =
  takeExtension . dropWhileEnd (\c -> c == dot || c == whitespace)
  where
    dot :: CHAR_TYPE
#if defined(OS_STRING)
    dot = OsString.fromWord 46
#else
    dot = '.'
#endif

    whitespace :: CHAR_TYPE
#if defined(OS_STRING)
    whitespace = OsString.fromWord 32
#else
    whitespace = ' '
#endif

-- Find CMD.EXE (or COMMAND.COM on Win98).  We use the same algorithm as
-- system() in the VC++ CRT (Vc7/crt/src/system.c in a VC++ installation).
findCommandInterpreter :: IO PATH_TYPE
findCommandInterpreter = do
  -- try COMSPEC first
  keyCOMSPEC <- encodeFS "COMSPEC"
  getEnv keyCOMSPEC >>= \case
    Nothing -> do
      -- try to find CMD.EXE or COMMAND.COM
      {-
      XXX We used to look at _osver (using cbits) and pick which shell to
      use with
      let filename | osver .&. 0x8000 /= 0 = "command.com"
                   | otherwise             = "cmd.exe"
      We ought to use GetVersionEx instead, but for now we just look for
      either filename
      -}
      cmdExe <- encodeFS "cmd.exe"
      commandCom <- encodeFS "command.com"

      let
          -- use our own version of System.Directory.findExecutable, because
          -- that assumes the .exe suffix.
          search :: [PATH_TYPE] -> IO (Maybe PATH_TYPE)
          search [] = return Nothing
          search (d:ds) = do
                  let path1 = d </> cmdExe
                      path2 = d </> commandCom
                  b1 <- doesFileExist path1
                  b2 <- doesFileExist path2
                  if b1 then return (Just path1)
                        else if b2 then return (Just path2)
                                   else search ds

      keyPATH <- encodeFS "PATH"
      getEnv keyPATH >>= \case
        Nothing -> notFound
        Just path -> do
          mb_path <- search (splitSearchPath path)
          case mb_path of
            Nothing -> notFound
            Just cmd -> return cmd
    Just cmd -> return cmd
  where
    notFound :: IO a
    notFound = ioError
      (mkIOError doesNotExistErrorType "findCommandInterpreter" Nothing Nothing)

-- | Alternative regime used to escape arguments destined for scripts
-- interpreted by @cmd.exe@, (e.g. @.bat@ and @.cmd@ files).
--
-- This respects the Windows command interpreter's quoting rules:
--
-- * the entire argument should be surrounded in quotes
-- * the backslash symbol is used to escape quotes and backslashes
-- * the carat symbol is used to escape other special characters with
--   significance to the interpreter
--
-- It is particularly important that we perform this quoting as
-- unvalidated unquoted command-line arguments can be used to achieve
-- arbitrary user code execution in when passed to a vulnerable batch
-- script.
--
translateCmdExeArg :: STRING_TYPE -> STRING_TYPE
#if defined(OS_STRING)
translateCmdExeArg xs =
  [osstr|^"|] <> snd (OsString.foldr escape (True, [osstr|^"|]) xs)
  where
    -- See long comment above for what this function is trying to do.
    --
    -- The Bool passed back along the string is True iff the
    -- rest of the string is a sequence of backslashes followed by
    -- a double quote.
    escape :: OsChar -> (Bool, OsString) -> (Bool, OsString)
    escape c = \case
      (_, string) | c == dquote -> (True, [osstr|\"|] <> string)
      (True, string) | c == backslash -> (True, [osstr|\\|] <> string)
      (False, string) | c == backslash -> (False, [osstr|\|] <> string)
      (_, string) | c == percent -> (False, [osstr|%%cd:~,%|] <> string)
      (_, string) | c `OsString.elem` [osstr|^<>|&()|] ->
        (False, OsString.cons caret (OsString.cons c string))
      (_, string) -> (False, OsString.cons c string)

    backslash :: OsChar
    backslash = OsString.fromWord 92

    caret :: OsChar
    caret = OsString.fromWord 94

    dquote :: OsChar
    dquote = OsString.fromWord 34

    percent :: OsChar
    percent = OsString.fromWord 37
#else
translateCmdExeArg xs = "^\"" ++ snd (foldr escape (True,"^\"") xs)
  where
    -- See long comment above for what this function is trying to do.
    --
    -- The Bool passed back along the string is True iff the
    -- rest of the string is a sequence of backslashes followed by
    -- a double quote.
    escape :: Char -> (Bool, String) -> (Bool, String)
    escape '"'  (_,     str) = (True,  '\\' : '"'  : str)
    escape '\\' (True,  str) = (True,  '\\' : '\\' : str)
    escape '\\' (False, str) = (False, '\\' : str)
    escape '%'  (_,     str) = (False, "%%cd:~,%" ++ str)
    escape c    (_,     str)
      | c `elem` "^<>|&()"   = (False, '^' : c : str)
      | otherwise            = (False,       c : str)
#endif

translateInternal :: STRING_TYPE -> STRING_TYPE
#if defined(OS_STRING)
translateInternal xs =
  [osstr|"|] <> snd (OsString.foldr escape (True, [osstr|"|]) xs)
  where
    -- See long comment above for what this function is trying to do.
    --
    -- The Bool passed back along the string is True iff the
    -- rest of the string is a sequence of backslashes followed by
    -- a double quote.
    escape :: OsChar -> (Bool, OsString) -> (Bool, OsString)
    escape c = \case
      (_, string) | c == dquote -> (True, [osstr|\"|] <> string)
      (True, string) | c == backslash -> (True, [osstr|\\|] <> string)
      (False, string) | c == backslash -> (False, [osstr|\|] <> string)
      (_, string) -> (False, OsString.cons c string)

    backslash :: OsChar
    backslash = OsString.fromWord 92

    dquote :: OsChar
    dquote = OsString.fromWord 34
#else
translateInternal xs = '"' : snd (foldr escape (True,"\"") xs)
  where
    -- See long comment above for what this function is trying to do.
    --
    -- The Bool passed back along the string is True iff the
    -- rest of the string is a sequence of backslashes followed by
    -- a double quote.
    escape :: Char -> (Bool, String) -> (Bool, String)
    escape '"'  (_,     str) = (True,  '\\' : '"'  : str)
    escape '\\' (True,  str) = (True,  '\\' : '\\' : str)
    escape '\\' (False, str) = (False, '\\' : str)
    escape c    (_,     str) = (False, c : str)
#endif

withCEnvironment
    :: [(STRING_TYPE, STRING_TYPE)] -> (Ptr CWString -> IO a) -> IO a
withCEnvironment environ action =
  let environ' =
        foldr
          (\(name, val) env0 -> name <> equalSign <> val <> nul <> env0)
          nul
          environ
  in useAsForeignString environ' (action . castPtr)
  where
    equalSign :: STRING_TYPE
#if defined(OS_STRING)
    equalSign = [osstr|=|]
#else
    equalSign = "="
#endif

    nul :: STRING_TYPE
#if defined(OS_STRING)
    nul = [osstr|\0|]
#else
    nul = "\0"
#endif

isDefaultSignal :: CLong -> Bool
isDefaultSignal = const False

createPipeInternal :: IO (Handle, Handle)
#if defined(__IO_MANAGER_WINIO__)
createPipeInternal = createPipeInternalPosix <!> createPipeInternalHANDLE
#else
createPipeInternal = createPipeInternalPosix
#endif

createPipeInternalPosix :: IO (Handle, Handle)
createPipeInternalPosix = do
    (readfd, writefd) <- createPipeInternalFd
    (do readh <- fdToHandle readfd
        writeh <- fdToHandle writefd
        return (readh, writeh)) `onException` (close' readfd >> close' writefd)

createPipeInternalFd :: IO (FD, FD)
createPipeInternalFd = do
    allocaArray 2 $ \ pfds -> do
        throwErrnoIfMinus1_ "_pipe" $ c__pipe pfds 8192 c_o_binary
        readfd <- peek pfds
        writefd <- peekElemOff pfds 1
        return (readfd, writefd)

#if defined(__IO_MANAGER_WINIO__)
createPipeInternalHANDLE :: IO (Handle, Handle)
createPipeInternalHANDLE =
  alloca $ \ pfdStdInput  ->
   alloca $ \ pfdStdOutput -> do
     throwErrnoIf_  (==False) "mkNamedPipe" $
       mkNamedPipe pfdStdInput True False pfdStdOutput True False
     Just hndStdInput  <- mbPipeHANDLE CreatePipe pfdStdInput ReadMode
     Just hndStdOutput <- mbPipeHANDLE CreatePipe pfdStdOutput WriteMode
     return (hndStdInput, hndStdOutput)

#endif

foreign import ccall "mkNamedPipe" mkNamedPipe ::
  Ptr HANDLE -> Bool -> Bool -> Ptr HANDLE -> Bool -> Bool -> IO Bool

close' :: CInt -> IO ()
close' = throwErrnoIfMinus1_ "_close" . c__close

foreign import ccall "io.h _pipe" c__pipe ::
    Ptr CInt -> CUInt -> CInt -> IO CInt

foreign import ccall "io.h _close" c__close ::
    CInt -> IO CInt

interruptProcessGroupOfInternal
    :: ProcessHandle    -- ^ A process in the process group
    -> IO ()
interruptProcessGroupOfInternal ph = do
    withProcessHandle ph $ \p_ -> do
        case p_ of
            ClosedHandle _ -> return ()
            _ -> do let h = phdlProcessHandle p_
                    pid <- getProcessId h
                    generateConsoleCtrlEvent cTRL_BREAK_EVENT pid
                    -- pgid <- getProcessGroupIDOf h
                    -- signalProcessGroup sigINT pgid
                    -- return ()

#if !defined(__IO_MANAGER_WINIO__)
type HANDLE = Ptr ()
#endif

#if defined(__IO_MANAGER_WINIO__)
-- It is not completely safe to pass the values -1 and -2 as HANDLE as it's an
-- unsigned type. -1 additionally is also the value for INVALID_HANDLE.  However
-- it should be safe in this case since an invalid handle would be an error here
-- anyway and the chances of us getting a handle with a value of -2 is
-- astronomical. However, sometime in the future process should really use a
-- proper structure here.
mbHANDLE :: HANDLE -> StdStream -> IO HANDLE
mbHANDLE _std CreatePipe      = return $ intPtrToPtr (-1)
mbHANDLE  std Inherit         = return std
mbHANDLE _std NoStream        = return $ intPtrToPtr (-2)
mbHANDLE _std (UseHandle hdl) = handleToHANDLE hdl

mbPipeHANDLE :: StdStream -> Ptr HANDLE -> IOMode -> IO (Maybe Handle)
mbPipeHANDLE CreatePipe pfd mode =
  Just <$> ( ( \ hANDLE -> rawHANDLEToHandle hANDLE mode ) =<< peek pfd )
mbPipeHANDLE _std      _pfd _mode = return Nothing

rawHANDLEToHandle :: HANDLE -> IOMode-> IO Handle
rawHANDLEToHandle raw_handle mode  = do
  let hwnd  = fromHANDLE raw_handle :: Io NativeHandle
      ident = "hwnd:" ++ show raw_handle
  enc <- getLocaleEncoding
  mkHandleFromHANDLE hwnd Stream ident mode (Just enc)
#endif
