{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module System.Process.Common
    ( CreateProcess (..)
    , CmdSpec (..)
    , StdStream (..)
    , ProcessHandle(..)
    , ProcessHandle__(..)
    , ProcRetHandles (..)
    , withFilePathException
    , PHANDLE
    , GroupID
    , UserID
    , modifyProcessHandle
    , withProcessHandle
    , fd_stdin
    , fd_stdout
    , fd_stderr
    , mbFd
    , mbPipe
    , pfdToHandle

-- Avoid a warning on Windows
#ifdef WINDOWS
    , CGid (..)
#else
    , CGid
#endif

-- WINIO is only available on GHC 8.12 and up.
#if defined(__IO_MANAGER_WINIO__)
    , HANDLE
    , mbHANDLE
    , mbPipeHANDLE
#endif
    ) where

import Control.Concurrent
import Control.Exception
import Data.String
import Foreign.Ptr
import Foreign.Storable

import System.Posix.Internals
import GHC.IO.Exception
import GHC.IO.Encoding
import qualified GHC.IO.FD as FD
import GHC.IO.Device
#if defined(__IO_MANAGER_WINIO__)
import GHC.IO.Handle.Windows
import GHC.IO.Windows.Handle (fromHANDLE, Io(), NativeHandle())
#endif
import GHC.IO.Handle.FD
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types hiding (ClosedHandle)
import System.IO.Error
import Data.Typeable
import System.IO (IOMode)

#if defined(javascript_HOST_ARCH)
import GHC.JS.Prim (JSVal)
#endif

-- We do a minimal amount of CPP here to provide uniform data types across
-- Windows and POSIX.
#ifdef WINDOWS
import Data.Word (Word32)
import System.Win32.DebugApi (PHANDLE)
#if defined(__IO_MANAGER_WINIO__)
import System.Win32.Types (HANDLE)
#endif
#else
import System.Posix.Types
#endif

#if defined(javascript_HOST_ARCH)
type PHANDLE = JSVal
#elif defined(WINDOWS)
-- Define some missing types for Windows compatibility. Note that these values
-- will never actually be used, as the setuid/setgid system calls are not
-- applicable on Windows. No value of this type will ever exist.
newtype CGid = CGid Word32
  deriving (Show, Eq)
type GroupID = CGid
type UserID = CGid
#else
type PHANDLE = CPid
#endif
data CreateProcess = CreateProcess{
  cmdspec      :: CmdSpec,                 -- ^ Executable & arguments, or shell command.  If 'cwd' is 'Nothing', relative paths are resolved with respect to the current working directory.  If 'cwd' is provided, it is implementation-dependent whether relative paths are resolved with respect to 'cwd' or the current working directory, so absolute paths should be used to ensure portability.
  cwd          :: Maybe FilePath,          -- ^ Optional path to the working directory for the new process
  env          :: Maybe [(String,String)], -- ^ Optional environment (otherwise inherit from the current process)
  std_in       :: StdStream,               -- ^ How to determine stdin
  std_out      :: StdStream,               -- ^ How to determine stdout
  std_err      :: StdStream,               -- ^ How to determine stderr
  close_fds    :: Bool,                    -- ^ Close all file descriptors except stdin, stdout and stderr in the new process (on Windows, only works if std_in, std_out, and std_err are all Inherit). This implementation will call close on every fd from 3 to the maximum of open files, which can be slow for high maximum of open files. XXX verify what happens with fds in nodejs child processes
  create_group :: Bool,                    -- ^ Create a new process group. On JavaScript this also creates a new session.
  delegate_ctlc:: Bool,                    -- ^ Delegate control-C handling. Use this for interactive console processes to let them handle control-C themselves (see below for details).
                                           --
                                           --   @since 1.2.0.0
  detach_console :: Bool,                  -- ^ Use the windows DETACHED_PROCESS flag when creating the process; does nothing on other platforms.
                                           --
                                           --   @since 1.3.0.0
  create_new_console :: Bool,              -- ^ Use the windows CREATE_NEW_CONSOLE flag when creating the process; does nothing on other platforms.
                                           --
                                           --   Default: @False@
                                           --
                                           --   @since 1.3.0.0
  new_session :: Bool,                     -- ^ Use posix setsid to start the new process in a new session; starts process in a new session on JavaScript; does nothing on other platforms.
                                           --
                                           --   @since 1.3.0.0
  child_group :: Maybe GroupID,            -- ^ Use posix setgid to set child process's group id; works for JavaScript when system running nodejs is posix. does nothing on other platforms.
                                           --
                                           --   Default: @Nothing@
                                           --
                                           --   @since 1.4.0.0
  child_user :: Maybe UserID,              -- ^ Use posix setuid to set child process's user id; works for JavaScript when system running nodejs is posix. does nothing on other platforms.
                                           --
                                           --   Default: @Nothing@
                                           --
                                           --   @since 1.4.0.0
  use_process_jobs :: Bool                 -- ^ On Windows systems this flag indicates that we should wait for the entire process tree
                                           --   to finish before unblocking. On POSIX systems this flag is ignored. See $exec-on-windows for details.
                                           --
                                           --   Default: @False@
                                           --
                                           --   @since 1.5.0.0
 } deriving (Show, Eq)

-- | contains the handles returned by a call to createProcess_Internal
data ProcRetHandles
  = ProcRetHandles { hStdInput      :: Maybe Handle
                   , hStdOutput     :: Maybe Handle
                   , hStdError      :: Maybe Handle
                   , procHandle     :: ProcessHandle
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
      --
      --   Windows does not have a mechanism for passing multiple arguments.
      --   When using @RawCommand@ on Windows, the command line is serialised
      --   into a string, with arguments quoted separately.  Command line
      --   parsing is up individual programs, so the default behaviour may
      --   not work for some programs.  If you are not getting the desired
      --   results, construct the command line yourself and use 'ShellCommand'.
      --
  deriving (Show, Eq)


-- | construct a `ShellCommand` from a string literal
--
-- @since 1.2.1.0
instance IsString CmdSpec where
  fromString = ShellCommand

data StdStream
  = Inherit                  -- ^ Inherit Handle from parent
  | UseHandle Handle         -- ^ Use the supplied Handle
  | CreatePipe               -- ^ Create a new pipe.  The returned
                             -- @Handle@ will use the default encoding
                             -- and newline translation mode (just
                             -- like @Handle@s created by @openFile@).
  | NoStream                 -- ^ Close the stream's file descriptor without
                             -- passing a Handle. On POSIX systems this may
                             -- lead to strange behavior in the child process
                             -- because attempting to read or write after the
                             -- file has been closed throws an error. This
                             -- should only be used with child processes that
                             -- don't use the file descriptor at all. If you
                             -- wish to ignore the child process's output you
                             -- should either create a pipe and drain it
                             -- manually or pass a @Handle@ that writes to
                             -- @\/dev\/null@.
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- ProcessHandle type

data ProcessHandle__ = OpenHandle { phdlProcessHandle :: PHANDLE }
                     -- | 'OpenExtHandle' is only applicable for
                     -- Windows platform. It represents [Job
                     -- Objects](https://learn.microsoft.com/en-us/windows/win32/procthread/job-objects).
                     | OpenExtHandle { phdlProcessHandle :: PHANDLE
                                     -- ^ the process
                                     , phdlJobHandle     :: PHANDLE
                                     -- ^ the job containing the process and
                                     -- its subprocesses
                                     }
                     | ClosedHandle ExitCode

{- | A handle to a process, which can be used to wait for termination
     of the process using 'System.Process.waitForProcess'.

     None of the process-creation functions in this library wait for
     termination: they all return a 'ProcessHandle' which may be used
     to wait for the process later.

     On Windows a second wait method can be used to block for event
     completion. This requires two handles. A process job handle and
     a events handle to monitor.
-}
data ProcessHandle
  = ProcessHandle { phandle          :: !(MVar ProcessHandle__)
                  , mb_delegate_ctlc :: !Bool
                  , waitpidLock      :: !(MVar ())
                  }

withFilePathException :: FilePath -> IO a -> IO a
withFilePathException fpath act = handle mapEx act
  where
    mapEx ex = ioError (ioeSetFileName ex fpath)

modifyProcessHandle
        :: ProcessHandle
        -> (ProcessHandle__ -> IO (ProcessHandle__, a))
        -> IO a
modifyProcessHandle (ProcessHandle m _ _) io = modifyMVar m io

withProcessHandle
        :: ProcessHandle
        -> (ProcessHandle__ -> IO a)
        -> IO a
withProcessHandle (ProcessHandle m _ _) io = withMVar m io

fd_stdin, fd_stdout, fd_stderr :: FD
fd_stdin  = 0
fd_stdout = 1
fd_stderr = 2

mbFd :: String -> FD -> StdStream -> IO FD
mbFd _   _std CreatePipe      = return (-1)
mbFd _fun std Inherit         = return std
mbFd _fn _std NoStream        = return (-2)
mbFd fun _std (UseHandle hdl) =
  withHandle fun hdl $ \Handle__{haDevice=dev,..} -> do
    case cast dev of
      Just fd -> do
#if !defined(javascript_HOST_ARCH)
         -- clear the O_NONBLOCK flag on this FD, if it is set, since
         -- we're exposing it externally (see #3316)
         fd' <- FD.setNonBlockingMode fd False
#else
         -- on the JavaScript platform we cannot change the FD flags
         fd' <- pure fd
#endif
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
mbPipeHANDLE CreatePipe pfd  mode =
  do raw_handle <- peek pfd
     let hwnd  = fromHANDLE raw_handle :: Io NativeHandle
         ident = "hwnd:" ++ show raw_handle
     enc <- fmap Just getLocaleEncoding
     Just <$> mkHandleFromHANDLE hwnd Stream ident mode enc
mbPipeHANDLE _std      _pfd _mode = return Nothing
#endif
