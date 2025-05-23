{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE InterruptibleFFI #-}

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
#if defined(mingw32_HOST_OS)
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
#if defined(mingw32_HOST_OS)
    terminateJob,
    terminateJobUnsafe,
    waitForJobCompletion,
    timeout_Infinite,
#else
#if !defined(javascript_HOST_ARCH)
    pPrPr_disableITimers, c_execvpe,
    runInteractiveProcess_lock,
#endif
    ignoreSignal, defaultSignal,
#endif
    withFilePathException, withCEnvironment,
    translate,
    createPipe,
    createPipeFd,
    interruptProcessGroupOf,
    withForkWait,
    ignoreSigPipe,
    ) where

import Control.Concurrent
import Control.Exception (SomeException, mask, try, throwIO)
import qualified Control.Exception as C
import Foreign.C
import System.IO

import GHC.IO.Exception ( IOErrorType(..), IOException(..) )
import GHC.IO.Handle.FD (fdToHandle)
import System.Posix.Internals (FD)

import System.Process.Common

#if defined(javascript_HOST_ARCH)
import System.Process.JavaScript
#elif defined(mingw32_HOST_OS)
import System.Process.Windows
#else
import System.Process.Posix
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
-- This function has been available from the "System.Process.Internals" module
-- for some time, and is part of the "System.Process" module since version
-- 1.2.1.0.
--
-- @since 1.2.1.0
createProcess_
  :: String
       -- ^ Function name (for error messages).
       --
       --   This can be any 'String', but will typically be the name of the caller.
       --   E.g., 'spawnProcess' passes @"spawnProcess"@ here when calling
       --   'createProcess_'.
  -> CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ msg proc_ = unwrapHandles `fmap` createProcess_Internal msg proc_
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
runGenProcess_ fun c (Just sig) (Just sig') | isDefaultSignal sig && sig == sig'
                         = createProcess_ fun c { delegate_ctlc = True }
runGenProcess_ fun c _ _ = createProcess_ fun c

-- ---------------------------------------------------------------------------
-- createPipe

-- | Create a pipe for interprocess communication and return a
-- @(readEnd, writeEnd)@ `Handle` pair.
--
-- * WinIO Support
--
-- When this function is used with WinIO enabled it's the caller's
-- responsibility to register the handles with the I/O manager.
-- If this is not done the operation will deadlock.  Association can
-- be done as follows:
--
-- @
--     #if defined(__IO_MANAGER_WINIO__)
--     import GHC.IO.SubSystem ((`<!>`))
--     import GHC.IO.Handle.Windows (handleToHANDLE)
--     import GHC.Event.Windows (associateHandle')
--     #endif
--
--     ...
--
--     #if defined(__IO_MANAGER_WINIO__)
--     return () \<!> do
--       associateHandle' =\<\< handleToHANDLE readEnd
--     #endif
-- @
--
-- Only associate handles that you are in charge of read/writing to.
-- Do not associate handles passed to another process.  It's the
-- process's responsibility to register the handle if it supports
-- async access.
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

-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
-- @since 1.6.20.0
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `C.onException` killThread tid

-- | Handle any SIGPIPE errors in the given computation.
--
-- @since 1.6.20.0
ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e