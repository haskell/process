{-# LANGUAGE CPP, ForeignFunctionInterface, RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -w #-}
-- XXX We get some warnings on Windows

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
-- Operations for creating and interacting with sub-processes.
--
-----------------------------------------------------------------------------

-- #hide
module System.Process.Internals (
#ifndef __HUGS__
	ProcessHandle(..), ProcessHandle__(..), 
	PHANDLE, closePHANDLE, mkProcessHandle, 
	withProcessHandle, withProcessHandle_,
#ifdef __GLASGOW_HASKELL__
        CreateProcess(..),
        CmdSpec(..), StdStream(..),
	runGenProcess_,
#endif
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
	 pPrPr_disableITimers, c_execvpe,
	ignoreSignal, defaultSignal,
#else
# ifdef __GLASGOW_HASKELL__
	translate,
# endif
#endif
#endif
	withFilePathException, withCEnvironment,

#ifndef __HUGS__
        fdToHandle,
#endif
  ) where

#ifndef __HUGS__
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import System.Posix.Types ( CPid )
import System.Posix.Process.Internals ( pPrPr_disableITimers, c_execvpe )
import System.IO 	( IOMode(..) )
#else
import Data.Word ( Word32 )
import Data.IORef
#endif
#endif

import System.IO 	( Handle )
import System.Exit	( ExitCode )
import Control.Concurrent
import Control.Exception
import Foreign.C
import Foreign

# ifdef __GLASGOW_HASKELL__

import System.Posix.Internals
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO.Exception
import GHC.IO.Encoding
import qualified GHC.IO.FD as FD
import GHC.IO.Device
import GHC.IO.Handle
import GHC.IO.Handle.FD
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import System.IO.Error
import Data.Typeable
#if defined(mingw32_HOST_OS)
import GHC.IO.IOMode
#endif
#else
import GHC.IOBase	( haFD, FD, IOException(..) )
import GHC.Handle
#endif

# elif __HUGS__

import Hugs.Exception	( IOException(..) )

# endif

#ifdef base4
import System.IO.Error		( ioeSetFileName )
#endif
#if defined(mingw32_HOST_OS)
import Control.Monad		( when )
import System.Directory		( doesFileExist )
import System.IO.Error		( isDoesNotExistError, doesNotExistErrorType,
				  mkIOError )
import System.Environment	( getEnv )
import System.FilePath
#endif

#ifdef __HUGS__
{-# CFILES cbits/execvpe.c  #-}
#endif

#include "HsProcessConfig.h"

#ifndef __HUGS__
-- ----------------------------------------------------------------------------
-- ProcessHandle type

{- | A handle to a process, which can be used to wait for termination
     of the process using 'waitForProcess'.

     None of the process-creation functions in this library wait for
     termination: they all return a 'ProcessHandle' which may be used
     to wait for the process later.
-}
data ProcessHandle__ = OpenHandle PHANDLE | ClosedHandle ExitCode
newtype ProcessHandle = ProcessHandle (MVar ProcessHandle__)

withProcessHandle
	:: ProcessHandle 
	-> (ProcessHandle__ -> IO (ProcessHandle__, a))
	-> IO a
withProcessHandle (ProcessHandle m) io = modifyMVar m io

withProcessHandle_
	:: ProcessHandle 
	-> (ProcessHandle__ -> IO ProcessHandle__)
	-> IO ()
withProcessHandle_ (ProcessHandle m) io = modifyMVar_ m io

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

type PHANDLE = CPid

mkProcessHandle :: PHANDLE -> IO ProcessHandle
mkProcessHandle p = do
  m <- newMVar (OpenHandle p)
  return (ProcessHandle m)

closePHANDLE :: PHANDLE -> IO ()
closePHANDLE _ = return ()

#else

type PHANDLE = Word32

-- On Windows, we have to close this HANDLE when it is no longer required,
-- hence we add a finalizer to it, using an IORef as the box on which to
-- attach the finalizer.
mkProcessHandle :: PHANDLE -> IO ProcessHandle
mkProcessHandle h = do
   m <- newMVar (OpenHandle h)
   addMVarFinalizer m (processHandleFinaliser m)
   return (ProcessHandle m)

processHandleFinaliser m =
   modifyMVar_ m $ \p_ -> do 
	case p_ of
	  OpenHandle ph -> closePHANDLE ph
	  _ -> return ()
	return (error "closed process handle")

closePHANDLE :: PHANDLE -> IO ()
closePHANDLE ph = c_CloseHandle ph

foreign import stdcall unsafe "CloseHandle"
  c_CloseHandle
	:: PHANDLE
	-> IO ()
#endif
#endif /* !__HUGS__ */

-- ----------------------------------------------------------------------------

data CreateProcess = CreateProcess{
  cmdspec   :: CmdSpec,                 -- ^ Executable & arguments, or shell command
  cwd       :: Maybe FilePath,          -- ^ Optional path to the working directory for the new process
  env       :: Maybe [(String,String)], -- ^ Optional environment (otherwise inherit from the current process)
  std_in    :: StdStream,               -- ^ How to determine stdin
  std_out   :: StdStream,               -- ^ How to determine stdout
  std_err   :: StdStream,               -- ^ How to determine stderr
  close_fds :: Bool                     -- ^ Close all file descriptors except stdin, stdout and stderr in the new process (on Windows, only works if std_in, std_out, and std_err are all Inherit)
 }

data CmdSpec 
  = ShellCommand String            
      -- ^ a command line to execute using the shell
  | RawCommand FilePath [String]
      -- ^ the filename of an executable with a list of arguments

data StdStream
  = Inherit                             -- ^ Inherit Handle from parent
  | UseHandle Handle                    -- ^ Use the supplied Handle
  | CreatePipe                          -- ^ Create a new pipe

runGenProcess_
  :: String                     -- ^ function name (for error messages)
  -> CreateProcess
  -> Maybe CLong		-- ^ handler for SIGINT
  -> Maybe CLong		-- ^ handler for SIGQUIT
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

#ifdef __GLASGOW_HASKELL__

-- -----------------------------------------------------------------------------
-- POSIX runProcess with signal handling in the child

runGenProcess_ fun CreateProcess{ cmdspec = cmdsp,
                                  cwd = mb_cwd,
                                  env = mb_env,
                                  std_in = mb_stdin,
                                  std_out = mb_stdout,
                                  std_err = mb_stderr,
                                  close_fds = mb_close_fds }
               mb_sigint mb_sigquit
 = do
  let (cmd,args) = commandToProcess cmdsp
  withFilePathException cmd $
   alloca $ \ pfdStdInput  ->
   alloca $ \ pfdStdOutput ->
   alloca $ \ pfdStdError  ->
   maybeWith withCEnvironment mb_env $ \pEnv ->
   maybeWith withCString mb_cwd $ \pWorkDir ->
   withMany withCString (cmd:args) $ \cstrs ->
   withArray0 nullPtr cstrs $ \pargs -> do
     
     fdin  <- mbFd fun fd_stdin  mb_stdin
     fdout <- mbFd fun fd_stdout mb_stdout
     fderr <- mbFd fun fd_stderr mb_stderr

     let (set_int, inthand) 
		= case mb_sigint of
			Nothing   -> (0, 0)
			Just hand -> (1, hand)
	 (set_quit, quithand) 
		= case mb_sigquit of
			Nothing   -> (0, 0)
			Just hand -> (1, hand)

     -- runInteractiveProcess() blocks signals around the fork().
     -- Since blocking/unblocking of signals is a global state
     -- operation, we better ensure mutual exclusion of calls to
     -- runInteractiveProcess().
     proc_handle <- withMVar runInteractiveProcess_lock $ \_ ->
                    throwErrnoIfMinus1 fun $
	                 c_runInteractiveProcess pargs pWorkDir pEnv 
                                fdin fdout fderr
				pfdStdInput pfdStdOutput pfdStdError
			        set_int inthand set_quit quithand
                                (if mb_close_fds then 1 else 0)

     hndStdInput  <- mbPipe mb_stdin  pfdStdInput  WriteMode
     hndStdOutput <- mbPipe mb_stdout pfdStdOutput ReadMode
     hndStdError  <- mbPipe mb_stderr pfdStdError  ReadMode

     ph <- mkProcessHandle proc_handle
     return (hndStdInput, hndStdOutput, hndStdError, ph)

{-# NOINLINE runInteractiveProcess_lock #-}
runInteractiveProcess_lock :: MVar ()
runInteractiveProcess_lock = unsafePerformIO $ newMVar ()

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
	-> CInt				-- non-zero: set child's SIGINT handler
	-> CLong			-- SIGINT handler
	-> CInt				-- non-zero: set child's SIGQUIT handler
	-> CLong			-- SIGQUIT handler
        -> CInt                         -- close_fds
        -> IO PHANDLE

#endif /* __GLASGOW_HASKELL__ */

ignoreSignal, defaultSignal :: CLong
ignoreSignal  = CONST_SIG_IGN
defaultSignal = CONST_SIG_DFL

#else

#ifdef __GLASGOW_HASKELL__

runGenProcess_ fun CreateProcess{ cmdspec = cmdsp,
                                  cwd = mb_cwd,
                                  env = mb_env,
                                  std_in = mb_stdin,
                                  std_out = mb_stdout,
                                  std_err = mb_stderr,
                                  close_fds = mb_close_fds }
               _ignored_mb_sigint _ignored_mb_sigquit
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
                    throwErrnoIfMinus1 fun $
	                 c_runInteractiveProcess pcmdline pWorkDir pEnv 
                                fdin fdout fderr
				pfdStdInput pfdStdOutput pfdStdError
                                (if mb_close_fds then 1 else 0)

     hndStdInput  <- mbPipe mb_stdin  pfdStdInput  WriteMode
     hndStdOutput <- mbPipe mb_stdout pfdStdOutput ReadMode
     hndStdError  <- mbPipe mb_stderr pfdStdError  ReadMode

     ph <- mkProcessHandle proc_handle
     return (hndStdInput, hndStdOutput, hndStdError, ph)

{-# NOINLINE runInteractiveProcess_lock #-}
runInteractiveProcess_lock :: MVar ()
runInteractiveProcess_lock = unsafePerformIO $ newMVar ()

foreign import ccall unsafe "runInteractiveProcess" 
  c_runInteractiveProcess
        :: CWString
        -> CWString
        -> Ptr ()
        -> FD
        -> FD
        -> FD
        -> Ptr FD
        -> Ptr FD
        -> Ptr FD
        -> CInt                         -- close_fds
        -> IO PHANDLE

-- ------------------------------------------------------------------------
-- Passing commands to the OS on Windows

{-
On Windows this is tricky.  We use CreateProcess, passing a single
command-line string (lpCommandLine) as its argument.  (CreateProcess
is well documented on http://msdn.microsoft.com.)

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

-- Translate command-line arguments for passing to CreateProcess().
translate :: String -> String
translate str = '"' : snd (foldr escape (True,"\"") str)
  where escape '"'  (b,     str) = (True,  '\\' : '"'  : str)
        escape '\\' (True,  str) = (True,  '\\' : '\\' : str)
        escape '\\' (False, str) = (False, '\\' : str)
	escape c    (b,     str) = (False, c : str)
	-- See long comment above for what this function is trying to do.
	--
	-- The Bool passed back along the string is True iff the
	-- rest of the string is a sequence of backslashes followed by
	-- a double quote.

#endif /* __GLASGOW_HASKELL__ */

#endif

fd_stdin, fd_stdout, fd_stderr :: FD
fd_stdin  = 0
fd_stdout = 1
fd_stderr = 2

mbFd :: String -> FD -> StdStream -> IO FD
mbFd _   _std CreatePipe      = return (-1)
mbFd _fun std Inherit         = return std
mbFd fun _std (UseHandle hdl) = 
#if __GLASGOW_HASKELL__ < 611
  withHandle_ fun hdl $ return . haFD
#else
  withHandle fun hdl $ \h@Handle__{haDevice=dev,..} ->
    case cast dev of
      Just fd -> do
         -- clear the O_NONBLOCK flag on this FD, if it is set, since
         -- we're exposing it externally (see #3316)
         fd <- FD.setNonBlockingMode fd False
         return (Handle__{haDevice=fd,..}, FD.fdFD fd)
      Nothing ->
          ioError (mkIOError illegalOperationErrorType
		      "createProcess" (Just hdl) Nothing
                   `ioeSetErrorString` "handle is not a file descriptor")
#endif

mbPipe :: StdStream -> Ptr FD -> IOMode -> IO (Maybe Handle)
mbPipe CreatePipe pfd  mode = fmap Just (pfdToHandle pfd mode)
mbPipe _std      _pfd _mode = return Nothing

pfdToHandle :: Ptr FD -> IOMode -> IO Handle
pfdToHandle pfd mode = do
  fd <- peek pfd
  let filepath = "fd:" ++ show fd
#if __GLASGOW_HASKELL__ >= 611
  (fD,fd_type) <- FD.mkFD (fromIntegral fd) mode 
                       (Just (Stream,0,0)) -- avoid calling fstat()
                       False {-is_socket-}
                       False {-non-blocking-}
  fD <- FD.setNonBlockingMode fD True -- see #3316
  mkHandleFromFD fD fd_type filepath mode False{-is_socket-}
                       (Just localeEncoding)
#else
  fdToHandle' fd (Just Stream)
     False{-Windows: not a socket,  Unix: don't set non-blocking-}
     filepath mode True{-binary-}
#endif

#ifndef __HUGS__
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
  return (cmd, translate cmd ++ "/c " ++ string)
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
#ifdef base3
  catchJust (\e -> case e of 
                     IOException e | isDoesNotExistError e -> Just e
                     _otherwise -> Nothing)
#else
  catchJust (\e -> if isDoesNotExistError e then Just e else Nothing)
#endif
            (getEnv "COMSPEC") $ \e -> do

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

#endif /* __HUGS__ */

-- ----------------------------------------------------------------------------
-- Utils

withFilePathException :: FilePath -> IO a -> IO a
withFilePathException fpath act = handle mapEx act
  where
#ifdef base4
    mapEx ex = ioError (ioeSetFileName ex fpath)
#else
    mapEx (IOException (IOError h iot fun str _)) = ioError (IOError h iot fun str (Just fpath))
#endif

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
withCEnvironment :: [(String,String)] -> (Ptr CString  -> IO a) -> IO a
withCEnvironment envir act =
  let env' = map (\(name, val) -> name ++ ('=':val)) envir 
  in withMany withCString env' (\pEnv -> withArray0 nullPtr pEnv act)
#else
withCEnvironment :: [(String,String)] -> (Ptr () -> IO a) -> IO a
withCEnvironment envir act =
  let env' = foldr (\(name, val) env -> name ++ ('=':val)++'\0':env) "\0" envir
  in withCString env' (act . castPtr)
#endif

