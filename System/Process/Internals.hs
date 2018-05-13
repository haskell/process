{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE InterruptibleFFI #-}

#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes, MagicHash #-}
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
#ifdef WINDOWS
    CGid(..),
#else
    CGid,
#endif
    GroupID,
    UserID,
    modifyProcessHandle, withProcessHandle,
    CreateProcess(..),
    CmdSpec(..), StdStream(..), ProcRetHandles (..),
    createProcess_,
    runGenProcess_, --deprecated
    fdToHandle,
    startDelegateControlC,
    endDelegateControlC,
    stopDelegateControlC,
    unwrapHandles,
#if !defined ghcjs_HOST_OS
#ifdef WINDOWS
    terminateJob,
    waitForJobCompletion,
    timeout_Infinite,
#else
    pPrPr_disableITimers, c_execvpe,
    ignoreSignal, defaultSignal,
#endif
#endif
    withFilePathException, withCEnvironment,
    translate,
    createPipe,
    createPipeFd,
    interruptProcessGroupOf,
    ) where

import Foreign.C
import System.IO

import GHC.IO.Handle.FD (fdToHandle)
import System.Posix.Internals (FD)

import System.Process.Common

#ifdef ghcjs_HOST_OS
import Control.Applicative
import Control.Concurrent.MVar
import GHCJS.Prim
import System.Exit
import System.IO.Error
import qualified GHC.IO.FD as FD
import GHC.IO.Handle.FD (mkHandleFromFD)
import GHC.IO.Device (IODeviceType(..))
import GHC.IO.Encoding (getLocaleEncoding)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Utils (withMany)
import Foreign.Marshal.Array (withArray0)

mkProcessHandle :: PHANDLE -> Bool -> IO ProcessHandle
mkProcessHandle p mb_delegate_ctlc = do
  m <- newMVar (OpenHandle p)
  ml <- newMVar ()
  return (ProcessHandle m mb_delegate_ctlc ml)

closePHANDLE :: PHANDLE -> IO ()
closePHANDLE _ = return ()

startDelegateControlC :: IO ()
startDelegateControlC = return ()

stopDelegateControlC :: IO ()
stopDelegateControlC = return ()

endDelegateControlC :: ExitCode -> IO ()
endDelegateControlC _ = return ()

isDefaultSignal :: CLong -> Bool
isDefaultSignal _ = True

interruptProcessGroupOfInternal
    :: ProcessHandle    -- ^ A process in the process group
    -> IO ()
interruptProcessGroupOfInternal ph =
  error "System.Process.interruptProcessGroupOfInternal: not yet supported for GHCJS"

translateInternal :: String -> String
translateInternal = id

createPipeInternal :: IO (Handle, Handle)
createPipeInternal = error "System.Process.createPipeInternal: not yet supported on GHCJS"

createPipeInternalFd :: IO (FD, FD)
createPipeInternalFd = error "System.Process.createPipeInternalFd: not yet supported on GHCJS"

withCEnvironment :: [(String,String)] -> (Ptr CString  -> IO a) -> IO a
withCEnvironment envir act =
  let env' = map (\(name, val) -> name ++ ('=':val)) envir
  in withMany withCString env' (\pEnv -> withArray0 nullPtr pEnv act)

{- -- fixme does ghcjs need anything special?
mbFd :: String -> FD -> StdStream -> IO FD
mbFd _ _std CreatePipe = return (-1)
mbFd _fun std Inherit  = return std
mbFd fun _std (UseHandle hdl) =
  withHandle fun hdl $ \x@Handle__{haDevice=dev,..} ->
    case cast dev of
      Just fd -> return (x, fd)
      Nothing -> ioError (mkIOError illegalOperationErrorType "createProcess" (Just hdl) Nothing
                   `ioeSetErrorString` "handle is not a file descriptor")

-}

commandToProcess :: CmdSpec -> IO (FilePath, [String])
commandToProcess (ShellCommand xs) = do
  r <- js_commandToProcess (toJSString xs) jsNull
  if isNull r
    then ioError (mkIOError doesNotExistErrorType "commandToProcess" Nothing Nothing)
    else (\(x:xs) -> (x,xs)) <$> fromJSStrings r
commandToProcess (RawCommand cmd args) = do
  r <- js_commandToProcess (toJSString cmd) =<< toJSStrings args
  if isNull r
    then ioError (mkIOError doesNotExistErrorType "commandToProcess" Nothing Nothing)
    else (\(x:xs) -> (x,xs)) <$> fromJSStrings r

#else
#ifdef WINDOWS
import System.Process.Windows
#else
import System.Process.Posix
#endif

#endif

-- ----------------------------------------------------------------------------
-- | This function is almost identical to
-- 'System.Process.createProcess'. The only differences are:
--
-- * 'Handle's provided via 'UseHandle' are not closed automatically.
--
-- * This function takes an extra @String@ argument to be used in creating
--   error messages.
--
-- * 'use_process_jobs' can be set in CreateProcess since 1.5.0.0 in order to create
--   an I/O completion port to monitor a process tree's progress on Windows.
--
-- The function also returns two new handles:
--   * an I/O Completion Port handle on which events
--     will be signaled.
--   * a Job handle which can be used to kill all running
--     processes.
--
--  On POSIX platforms these two new handles will always be Nothing
--
--
-- This function has been available from the "System.Process.Internals" module
-- for some time, and is part of the "System.Process" module since version
-- 1.2.1.0.
--
-- @since 1.2.1.0
createProcess_
  :: String                     -- ^ function name (for error messages)
  -> CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
#ifdef ghcjs_HOST_OS
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
  (cmd,args) <- commandToProcess cmdsp
  withFilePathException cmd $ do
     fdin  <- mbFd fun fd_stdin  mb_stdin
     fdout <- mbFd fun fd_stdout mb_stdout
     fderr <- mbFd fun fd_stderr mb_stderr
     env'  <- maybe (return jsNull) (toJSStrings . concatMap (\(x,y) -> [x,y])) mb_env
     let cwd' = maybe jsNull toJSString mb_cwd
     let c1 = toJSString cmd
     c2 <- case args of
               [] -> return jsNull
               _  -> toJSStrings args

     r <- js_runInteractiveProcess c1 c2 cwd' env' fdin fdout fderr
         mb_close_fds mb_create_group mb_delegate_ctlc

     proc_handle <- fromIntegral . fromJSInt <$> getProp r "pid"
     fds@[fdin_r, fdout_r, fderr_r] <- map (stdFD . fromIntegral) <$> (fromJSInts =<< getProp r "fds")

     hndStdInput  <- mbPipe_GHCJS mb_stdin  fdin_r  WriteMode
     hndStdOutput <- mbPipe_GHCJS mb_stdout fdout_r ReadMode
     hndStdError  <- mbPipe_GHCJS mb_stderr fderr_r ReadMode

     ph <- mkProcessHandle proc_handle mb_delegate_ctlc
     return (hndStdInput, hndStdOutput, hndStdError, ph)

mbPipe_GHCJS :: StdStream -> FD.FD -> IOMode -> IO (Maybe Handle)
mbPipe_GHCJS CreatePipe fd mode = do
{-  (fD,fd_type) <- FD.mkFD (fromIntegral fd) mode
                       (Just (Stream,0,0)) -- avoid calling fstat()
                       False {-is_socket-}
                       False {-non-blocking-} -}
  enc <- getLocaleEncoding
  fmap Just (mkHandleFromFD fd Stream ("fd: " ++ show fd) mode False {-is_socket-} (Just enc))
mbPipe_GHCJS _ _ _ = return Nothing


stdFD :: CInt -> FD.FD
stdFD fd = FD.FD { FD.fdFD = fd,
#ifdef mingw32_HOST_OS
                FD.fdIsSocket_ = 0
#else
                FD.fdIsNonBlocking = 0
   -- We don't set non-blocking mode on standard handles, because it may
   -- confuse other applications attached to the same TTY/pipe
   -- see Note [nonblock]
#endif
                  }

#else
createProcess_ msg proc_ = unwrapHandles `fmap` createProcess_Internal msg proc_
#endif
{-# INLINE createProcess_ #-}

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

           https://msdn.microsoft.com/en-us/library/a1y7w461.aspx

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
translate = translateInternal
{-# INLINE translate #-}

-- ---------------------------------------------------------------------------
-- unwrapHandles
unwrapHandles :: ProcRetHandles -> (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
unwrapHandles r = (hStdInput r, hStdOutput r, hStdError r, procHandle r)

#if defined(ghcjs_HOST_OS)

type JSArray  = JSVal
type JSObject = JSVal
type JSString = JSVal

fromJSStrings :: JSVal -> IO [String]
fromJSStrings x = fmap (map fromJSString) (fromJSArray x)

fromJSInts :: JSVal -> IO [Int]
fromJSInts x = map fromJSInt <$> fromJSArray x

toJSStrings :: [String] -> IO JSVal
toJSStrings xs = toJSArray (map toJSString xs)

throwErrnoIfJSNull :: String -> IO JSVal -> IO JSVal
throwErrnoIfJSNull msg m = do
  r <- m
  if isNull r then throwErrno msg
              else return r

foreign import javascript safe
  "h$process_runInteractiveProcess($1,$2,$3,$4,$5,$6,$7,$8,$9,$10)"
  js_runInteractiveProcess :: JSString     -- ^ $1 command or program
                           -> JSArray      -- ^ $2 arguments, null if it's a raw command
                           -> JSString     -- ^ $3 working dir, null for current
                           -> JSArray      -- ^ $4 environment, null for existing
                           -> CInt         -- ^ $5 stdin fd
                           -> CInt         -- ^ $6 stdout fd
                           -> CInt         -- ^ $7 stderr fd
                           -> Bool         -- ^ $8 close handles
                           -> Bool         -- ^ $9
                           -> Bool         -- ^ $10 delegate ctrl-c
                           -> IO JSVal     -- ^ process handle

foreign import javascript safe
  "h$process_commandToProcess($1,$2)"
  js_commandToProcess :: JSString -> JSArray -> IO JSArray
#endif

-- ----------------------------------------------------------------------------
-- Deprecated / compat

{-# DEPRECATED runGenProcess_
      "Please do not use this anymore, use the ordinary 'System.Process.createProcess'. If you need the SIGINT handling, use delegate_ctlc = True (runGenProcess_ is now just an imperfectly emulated stub that probably duplicates or overrides your own signal handling)." #-}
runGenProcess_
 :: String                     -- ^ function name (for error messages)
 -> CreateProcess
 -> Maybe CLong                -- ^ handler for SIGINT
 -> Maybe CLong                -- ^ handler for SIGQUIT
 -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
-- On Windows, setting delegate_ctlc has no impact
runGenProcess_ fun c (Just sig) (Just sig') | isDefaultSignal sig && sig == sig'
                         = createProcess_ fun c { delegate_ctlc = True }
runGenProcess_ fun c _ _ = createProcess_ fun c

-- ---------------------------------------------------------------------------
-- createPipe

-- | Create a pipe for interprocess communication and return a
-- @(readEnd, writeEnd)@ `Handle` pair.
--
-- @since 1.2.1.0
createPipe :: IO (Handle, Handle)
createPipe = createPipeInternal
{-# INLINE createPipe #-}

-- ---------------------------------------------------------------------------
-- createPipeFd

-- | Create a pipe for interprocess communication and return a
-- @(readEnd, writeEnd)@ `FD` pair.
--
-- @since 1.4.2.0
createPipeFd :: IO (FD, FD)
createPipeFd = createPipeInternalFd
{-# INLINE createPipeFd #-}


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
interruptProcessGroupOf = interruptProcessGroupOfInternal
