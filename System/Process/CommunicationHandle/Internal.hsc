{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module System.Process.CommunicationHandle.Internal
  ( -- * 'CommunicationHandle': a 'Handle' that can be serialised,
    -- enabling inter-process communication.
    CommunicationHandle(..)
  , closeCommunicationHandle
    -- ** Internal functions
  , useCommunicationHandle
  , createCommunicationPipe
  )
 where

import Control.Arrow ( first )
import GHC.IO.Handle (Handle, hClose)
#if defined(mingw32_HOST_OS)
import Foreign.C (CInt(..), throwErrnoIf_)
import Foreign.Marshal (alloca)
import Foreign.Ptr (ptrToWordPtr, wordPtrToPtr)
import Foreign.Storable (Storable(peek))
import GHC.IO.Handle.FD (fdToHandle)
import GHC.IO.IOMode (IOMode(ReadMode, WriteMode))
import System.Process.Windows (HANDLE, mkNamedPipe)
##  if defined(__IO_MANAGER_WINIO__)
import Control.Exception (catch, throwIO)
import GHC.IO (onException)
import GHC.IO.Device as IODevice (close, devType)
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.Exception (IOException(..), IOErrorType(InvalidArgument))
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import GHC.IO.Handle.Windows (mkHandleFromHANDLE)
import GHC.IO.SubSystem ((<!>))
import GHC.IO.Windows.Handle (Io, NativeHandle, fromHANDLE)
import GHC.Event.Windows (associateHandle')
import System.Process.Common (rawHANDLEToHandle)
##  else
import System.Process.Common (rawFdToHandle)
##  endif

#include <fcntl.h>     /* for _O_BINARY */

#else
import GHC.IO.FD
  ( mkFD, setNonBlockingMode )
import GHC.IO.Handle
  ( noNewlineTranslation )
#if MIN_VERSION_base(4,16,0)
import GHC.IO.Handle.Internals
  ( mkFileHandleNoFinalizer )
#else
import GHC.IO.IOMode
  ( IOMode(..) )
import GHC.IO.Handle.Types
  ( HandleType(..) )
import GHC.IO.Handle.Internals
  ( mkHandle )
#endif
import System.Posix
  ( Fd(..)
  , FdOption(..), setFdOption
  )
import System.Posix.Internals
  ( fdGetMode )
import System.Process.Internals
  ( createPipeFd )
#endif

--------------------------------------------------------------------------------
-- Communication handles.

-- | A 'CommunicationHandle' is an abstraction over operating-system specific
-- internal representation of a 'Handle', which can be communicated through a
-- command-line interface.
--
-- In a typical use case, the parent process creates a pipe, using e.g.
-- 'createWeReadTheyWritePipe' or 'createTheyReadWeWritePipe'.
--
--  - One end of the pipe is a 'Handle', which can be read from/written to by
--    the parent process.
--  - The other end is a 'CommunicationHandle', which can be inherited by a
--    child process. A reference to the handle can be serialised (using
--    the 'Show' instance), and passed to the child process.
--    It is recommended to close the parent's reference to the 'CommunicationHandle'
--    using 'closeCommunicationHandle' after it has been inherited by the child
--    process.
--  - The child process can deserialise the 'CommunicationHandle' (using
--    the 'Read' instance), and then use 'openCommunicationHandleWrite' or
--    'openCommunicationHandleRead' in order to retrieve a 'Handle' which it
--    can write to/read from.
--
-- 'readCreateProcessWithExitCodeCommunicationHandle' provides a high-level API
-- to this functionality. See there for example code.
--
-- @since 1.6.20.0
newtype CommunicationHandle =
  CommunicationHandle
##if defined(mingw32_HOST_OS)
    HANDLE
##else
    Fd
##endif
  deriving ( Eq, Ord )

#if defined(mingw32_HOST_OS)
type Fd = CInt
#endif

-- @since 1.6.20.0
instance Show CommunicationHandle where
  showsPrec p (CommunicationHandle h) =
    showsPrec p
##if defined(mingw32_HOST_OS)
      $ ptrToWordPtr
##endif
      h

-- @since 1.6.20.0
instance Read CommunicationHandle where
  readsPrec p str =
    fmap
      ( first $ CommunicationHandle
##if defined(mingw32_HOST_OS)
              . wordPtrToPtr
##endif
      ) $
      readsPrec p str

-- | Internal function used to define 'openCommunicationHandleRead' and
-- openCommunicationHandleWrite.
useCommunicationHandle :: Bool -> CommunicationHandle -> IO Handle
useCommunicationHandle _wantToRead (CommunicationHandle ch) = do
##if defined(__IO_MANAGER_WINIO__)
  return ()
    <!> associateHandleWithFallback _wantToRead ch
##endif
  getGhcHandle ch

-- | Close a 'CommunicationHandle'.
--
-- Use this to close the 'CommunicationHandle' in the parent process after
-- the 'CommunicationHandle' has been inherited by the child process.
--
-- @since 1.6.20.0
closeCommunicationHandle :: CommunicationHandle -> IO ()
closeCommunicationHandle (CommunicationHandle ch) =
  hClose =<< getGhcHandle ch

##if defined(__IO_MANAGER_WINIO__)
-- Internal function used when associating a 'HANDLE' with the current process.
--
-- Explanation: with WinIO, a synchronous handle cannot be associated with the
-- current process, while an asynchronous one must be associated before being usable.
--
-- In a child process, we don't necessarily know which kind of handle we will receive,
-- so we try to associate it (in case it is an asynchronous handle). This might
-- fail (if the handle is synchronous), in which case we continue in synchronous
-- mode (without associating).
--
-- With the current API, inheritable handles in WinIO created with mkNamedPipe
-- are synchronous, but it's best to be safe in case the child receives an
-- asynchronous handle anyway.
associateHandleWithFallback :: Bool -> HANDLE -> IO ()
associateHandleWithFallback _wantToRead h =
  associateHandle' h `catch` handler
  where
    handler :: IOError -> IO ()
    handler ioErr@(IOError { ioe_handle = _mbErrHandle, ioe_type = errTy, ioe_errno = mbErrNo })
      -- Catches the following error that occurs when attemping to associate
      -- a HANDLE that does not have OVERLAPPING mode set:
      --
      --   associateHandleWithIOCP: invalid argument (The parameter is incorrect.)
      | InvalidArgument <- errTy
      , Just 22 <- mbErrNo
      = return ()
      | otherwise
      = throwIO ioErr
##endif

-- | Gets a GHC Handle File description from the given OS Handle or POSIX fd.

#if defined(mingw32_HOST_OS)
getGhcHandle :: HANDLE -> IO Handle
getGhcHandle =
  getGhcHandlePOSIX
##  if defined(__IO_MANAGER_WINIO__)
    <!> getGhcHandleNative
##  endif

getGhcHandlePOSIX :: HANDLE -> IO Handle
getGhcHandlePOSIX handle = openHANDLE handle >>= fdToHandle

openHANDLE :: HANDLE -> IO Fd
openHANDLE handle = _open_osfhandle handle (#const _O_BINARY)

foreign import ccall "io.h _open_osfhandle"
  _open_osfhandle :: HANDLE -> CInt -> IO Fd

##  if defined(__IO_MANAGER_WINIO__)
getGhcHandleNative :: HANDLE -> IO Handle
getGhcHandleNative hwnd =
  do mb_codec <- fmap Just getLocaleEncoding
     let iomode = ReadWriteMode
         native_handle = fromHANDLE hwnd :: Io NativeHandle
     hw_type <- IODevice.devType $ native_handle
     mkHandleFromHANDLE native_handle hw_type (show hwnd) iomode mb_codec
       `onException` IODevice.close native_handle
##  endif
#else
getGhcHandle :: Fd -> IO Handle
getGhcHandle (Fd fdint) = do
  iomode <- fdGetMode fdint
  (fd0, _) <- mkFD fdint iomode Nothing False True
  -- The following copies over 'mkHandleFromFDNoFinalizer'
  fd <- setNonBlockingMode fd0 True
  let fd_str = "<file descriptor: " ++ show fd ++ ">"
#  if MIN_VERSION_base(4,16,0)
  mkFileHandleNoFinalizer fd fd_str iomode Nothing noNewlineTranslation
#  else
  mkHandle fd fd_str (ioModeToHandleType iomode) True Nothing noNewlineTranslation
    Nothing Nothing

ioModeToHandleType :: IOMode -> HandleType
ioModeToHandleType mode =
  case mode of
    ReadMode      -> ReadHandle
    WriteMode     -> WriteHandle
    ReadWriteMode -> ReadWriteHandle
    AppendMode    -> AppendHandle
#  endif
#endif

--------------------------------------------------------------------------------
-- Creating pipes.

-- | Internal helper function used to define 'createWeReadTheyWritePipe'
-- and 'createTheyReadWeWritePipe' while reducing code duplication.
--
-- The returned 'Handle' does not have any finalizers attached to it;
-- use 'hClose' to close it.
createCommunicationPipe
  :: ( forall a. (a, a) -> (a, a) )
    -- ^ 'id' (we read, they write) or 'swap' (they read, we write)
  -> Bool -- ^ whether to pass a handle supporting asynchronous I/O to the child process
          -- (this flag only has an effect on Windows and when using WinIO)
  -> IO (Handle, CommunicationHandle)
createCommunicationPipe swapIfTheyReadWeWrite _passAsyncHandleToChild = do
##if !defined(mingw32_HOST_OS)
  -- NB: it's important to use 'createPipeFd' here.
  --
  -- Were we to instead use 'createPipe', we would create a Handle for both pipe
  -- ends, including the end we pass to the child.
  -- Such Handle would have a finalizer which closes the underlying file descriptor.
  -- However, we will already close the FD after it is inherited by the child.
  -- This could lead to the following scenario:
  --
  --  - the parent creates a new pipe, e.g. pipe2([7,8]),
  --  - the parent spawns a child process, and lets FD 8 be inherited by the child,
  --  - the parent closes FD 8,
  --  - the parent opens FD 8 for some other purpose, e.g. for writing to a file,
  --  - the finalizer for the Handle wrapping FD 8 runs, closing FD 8, even though
  --    it is now in use for a completely different purpose.
  (ourFd, theirFd) <- swapIfTheyReadWeWrite <$> createPipeFd
  -- Don't allow the child process to inherit a parent file descriptor
  -- (such inheritance happens by default on Unix).
  setFdOption (Fd ourFd) CloseOnExec True
  -- NB: we will be closing this handle manually, so don't use 'handleFromFd'
  -- which attaches a finalizer that closes the FD. See the above comment
  -- about 'createPipeFd'.
  ourHandle <- getGhcHandle (Fd ourFd)
  return (ourHandle, CommunicationHandle $ Fd theirFd)
##else
  trueForWinIO <-
    return False
##  if defined (__IO_MANAGER_WINIO__)
      <!> return True
##  endif
  -- On Windows, use mkNamedPipe to create the two pipe ends.
  alloca $ \ pfdStdInput  ->
    alloca $ \ pfdStdOutput -> do
      let (inheritRead, inheritWrite) = swapIfTheyReadWeWrite (False, True)
          -- WinIO:
          --  - make the parent pipe end overlapped,
          --  - make the child end overlapped if requested,
          -- Otherwise: make both pipe ends synchronous.
          overlappedRead  = trueForWinIO && ( _passAsyncHandleToChild || not inheritRead  )
          overlappedWrite = trueForWinIO && ( _passAsyncHandleToChild || not inheritWrite )
      throwErrnoIf_ (==False) "mkNamedPipe" $
        mkNamedPipe
          pfdStdInput  inheritRead  overlappedRead
          pfdStdOutput inheritWrite overlappedWrite
      let ((ourPtr, ourMode), (theirPtr, _theirMode)) =
            swapIfTheyReadWeWrite ((pfdStdInput, ReadMode), (pfdStdOutput, WriteMode))
      ourHANDLE  <- peek ourPtr
      theirHANDLE <- peek theirPtr
      -- With WinIO, we need to associate any handles we are going to use in
      -- the current process before being able to use them.
      return ()
##  if defined (__IO_MANAGER_WINIO__)
        <!> associateHandle' ourHANDLE
##  endif
      ourHandle <-
##  if !defined (__IO_MANAGER_WINIO__)
        ( \ fd -> rawFdToHandle fd ourMode ) =<< openHANDLE ourHANDLE
##  else
        -- NB: it's OK to call the following function even when we're not
        -- using WinIO at runtime, so we don't use <!>.
        rawHANDLEToHandle ourHANDLE ourMode
##  endif
      return $ (ourHandle, CommunicationHandle theirHANDLE)
##endif
