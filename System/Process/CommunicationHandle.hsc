{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module System.Process.CommunicationHandle
  ( -- * 'CommunicationHandle': a 'Handle' that can be serialised,
    -- enabling inter-process communication.
    CommunicationHandle
      -- NB: opaque, as the representation depends on the operating system
  , openCommunicationHandleRead
  , openCommunicationHandleWrite
  , closeCommunicationHandle
    -- * Creating 'CommunicationHandle's to communicate with
    -- a child process
  , createWeReadTheyWritePipe
  , createTheyReadWeWritePipe
   -- * High-level API
  , readCreateProcessWithExitCodeCommunicationHandle
  )
 where

import Control.Arrow ( first )
import Foreign.C (CInt(..), throwErrnoIf_)
import GHC.IO.Handle (Handle())
#if defined(mingw32_HOST_OS)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, ptrToWordPtr, wordPtrToPtr)
import Foreign.Storable (Storable(peek))
import GHC.IO.Handle.FD (fdToHandle)
import GHC.IO.IOMode (IOMode(ReadMode, WriteMode))
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
import GHC.Windows (HANDLE)
import System.Process.Common (rawHANDLEToHandle)
##  else
import System.Process.Common (rawFdToHandle)
##  endif

#include <fcntl.h>     /* for _O_BINARY */

#else
import System.Posix
  ( Fd(..), fdToHandle
  , FdOption(..), setFdOption
  )
import GHC.IO.FD (FD(fdFD))
-- NB: we use GHC.IO.Handle.Fd.handleToFd rather than System.Posix.handleToFd,
-- as the latter flushes and closes the `Handle`, which is not the behaviour we want.
import GHC.IO.Handle.FD (handleToFd)
#endif

import System.Process.Internals
  ( CreateProcess(..), ignoreSigPipe, withForkWait,
##if !defined(mingw32_HOST_OS)
  createPipe
##endif
  )
import System.Process
  ( withCreateProcess, waitForProcess )

import GHC.IO (evaluate)
import GHC.IO.Handle (hClose)
import System.Exit (ExitCode)

import Control.DeepSeq (NFData, rnf)

--------------------------------------------------------------------------------
-- Communication handles.

-- | A 'CommunicationHandle' is an operating-system specific representation
-- of a 'Handle' that can be communicated through a command-line interface.
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
--    the 'Read' instance), and then use 'useCommunicationHandle'
--    in order to retrieve a 'Handle' which it can write to/read from.
--
-- 'readCreateProcessWithExitCodeCommunicationHandle' provides a high-level API
-- to this functionality. See there for example code.
--
-- @since 1.6.19.0
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
##  if !defined(__IO_MANAGER_WINIO__)
type HANDLE = Ptr ()
##  endif
#endif

-- @since 1.6.19.0
instance Show CommunicationHandle where
  showsPrec p (CommunicationHandle h) =
    showsPrec p
##if defined(mingw32_HOST_OS)
      $ ptrToWordPtr
##endif
      h

-- @since 1.6.19.0
instance Read CommunicationHandle where
  readsPrec p str =
    fmap
      ( first $ CommunicationHandle
##if defined(mingw32_HOST_OS)
              . wordPtrToPtr
##endif
      ) $
      readsPrec p str

-- | Turn the 'CommunicationHandle' into a 'Handle' that can be read from
-- in the current process.
--
-- @since 1.6.19.0
openCommunicationHandleRead :: CommunicationHandle -> IO Handle
openCommunicationHandleRead = useCommunicationHandle True

-- | Turn the 'CommunicationHandle' into a 'Handle' that can be written to
-- in the current process.
--
-- @since 1.6.19.0
openCommunicationHandleWrite :: CommunicationHandle -> IO Handle
openCommunicationHandleWrite = useCommunicationHandle False

-- | Internal function used to define 'openCommunicationHandleRead' and
-- openCommunicationHandleWrite.
useCommunicationHandle :: Bool -> CommunicationHandle -> IO Handle
useCommunicationHandle wantToRead (CommunicationHandle ch) = do
##if defined(__IO_MANAGER_WINIO__)
  return ()
    <!> associateHandleWithFallback wantToRead ch
##endif
  getGhcHandle ch

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

-- | Close a 'CommunicationHandle'.
--
-- Use this to close the 'CommunicationHandle' in the parent process after
-- the 'CommunicationHandle' has been inherited by the child process.
--
-- @since 1.6.19.0
closeCommunicationHandle :: CommunicationHandle -> IO ()
closeCommunicationHandle (CommunicationHandle ch) =
  hClose =<< getGhcHandle ch

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
getGhcHandle fd = fdToHandle fd
#endif

--------------------------------------------------------------------------------
-- Creating pipes.

-- | Create a pipe @(weRead,theyWrite)@ that the current process can read from,
-- and whose write end can be passed to a child process in order to receive data from it.
--
-- See 'CommunicationHandle'.
--
-- @since 1.6.19.0
createWeReadTheyWritePipe :: IO (Handle, CommunicationHandle)
createWeReadTheyWritePipe = createCommunicationPipe id

-- | Create a pipe @(theyRead,weWrite)@ that the current process can write to,
-- and whose read end can be passed to a child process in order to send data to it.
--
-- See 'CommunicationHandle'.
--
-- @since 1.6.19.0
createTheyReadWeWritePipe :: IO (CommunicationHandle, Handle)
createTheyReadWeWritePipe = sw <$> createCommunicationPipe sw
  where
    sw (a,b) = (b,a)

-- | Internal helper function used to define 'createWeReadTheyWritePipe'
-- and 'createTheyReadWeWritePipe' while reducing code duplication.
createCommunicationPipe
  :: ( forall a. (a, a) -> (a, a) )
  -> IO (Handle, CommunicationHandle)
createCommunicationPipe mbSwap = do
##if !defined(mingw32_HOST_OS)
  (ourHandle, theirHandle) <- mbSwap <$> createPipe
  -- Don't allow the child process to inherit a parent file descriptor
  -- (such inheritance happens by default on Unix).
  ourFD   <- Fd . fdFD <$> handleToFd ourHandle
  setFdOption ourFD CloseOnExec True
  theirFD <- Fd . fdFD <$> handleToFd theirHandle
  return (ourHandle, CommunicationHandle theirFD)
##else
  trueForWinIO <-
    return False
##  if defined (__IO_MANAGER_WINIO__)
      <!> return True
##  endif
  -- On Windows, use mkNamedPipe to create the two pipe ends.
  alloca $ \ pfdStdInput  ->
    alloca $ \ pfdStdOutput -> do
      let (inheritRead, inheritWrite) = mbSwap (False, True)
          -- If we're using WinIO, make the parent pipe end overlapped,
          -- otherwise make both pipe ends synchronous.
          overlappedRead  = if inheritRead  then False else trueForWinIO
          overlappedWrite = if inheritWrite then False else trueForWinIO
      throwErrnoIf_ (==False) "c_mkNamedPipe" $
        -- Create one end to be un-inheritable and the other
        -- to be inheritable, which ensures the parent end can be properly
        -- associated with the parent process.
        c_mkNamedPipe
          pfdStdInput  inheritRead  overlappedRead
          pfdStdOutput inheritWrite overlappedWrite
      let ((ourPtr, ourMode), (theirPtr, _theirMode)) =
            mbSwap ((pfdStdInput, ReadMode), (pfdStdOutput, WriteMode))
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
        rawFdToHandle ourMode =<< openHANDLE ourHANDLE
##  else
        -- NB: it's OK to call the following function even when we're not
        -- using WinIO at runtime, so we don't use <!>.
        rawHANDLEToHandle ourMode ourHANDLE
##  endif
      return $ (ourHandle, CommunicationHandle theirHANDLE)

foreign import ccall "mkNamedPipe" c_mkNamedPipe ::
    Ptr HANDLE -> Bool -> Bool -> Ptr HANDLE -> Bool -> Bool -> IO Bool

##endif

--------------------------------------------------------------------------------

-- | A version of 'readCreateProcessWithExitCode' that communicates with the
-- child process through a pair of 'CommunicationHandle's.
--
-- Example usage:
--
-- > readCreateProcessWithExitCodeCommunicationHandle
-- >   (\(chTheyRead, chTheyWrite) -> proc "child-exe" [show chTheyRead, show chTheyWrite])
-- >   (\ hWeRead -> hGetContents hWeRead)
-- >   (\ hWeWrite -> hPut hWeWrite "xyz")
--
-- where @child-exe@ is a separate executable that is implemented as:
--
-- > main = do
-- >   [chRead, chWrite] <- getArgs
-- >   hRead  <- useCommunicationHandle $ read chRead
-- >   hWrite <- useCommunicationHandle $ read chWrite
-- >   input <- hGetContents hRead
-- >   hPut hWrite $ someFn input
-- >   hClose hWrite
--
-- @since 1.6.19.0
readCreateProcessWithExitCodeCommunicationHandle
  :: NFData a
  => ((CommunicationHandle, CommunicationHandle) -> CreateProcess)
    -- ^ Process to spawn, given a @(read, write)@ pair of
    -- 'CommunicationHandle's that are inherited by the spawned process
  -> (Handle -> IO a)
    -- ^ read action
  -> (Handle -> IO ())
    -- ^ write action
  -> IO (ExitCode, a)
readCreateProcessWithExitCodeCommunicationHandle mkProg readAction writeAction = do
  (chTheyRead, hWeWrite   ) <- createTheyReadWeWritePipe
  (hWeRead   , chTheyWrite) <- createWeReadTheyWritePipe
  let cp = mkProg (chTheyRead, chTheyWrite)
  -- The following implementation parallels 'readCreateProcess'
  withCreateProcess cp $ \ _ _ _ ph -> do
    -- Close the parent's references to the 'CommunicationHandle's after they
    -- have been inherited by the child (we don't want to keep pipe ends open).
    closeCommunicationHandle chTheyWrite
    closeCommunicationHandle chTheyRead

    -- Fork off a thread that waits on the output.
    output <- readAction hWeRead
    withForkWait (evaluate $ rnf output) $ \ waitOut -> do
      ignoreSigPipe $ writeAction hWeWrite
      ignoreSigPipe $ hClose hWeWrite
      waitOut
      hClose hWeRead

    ex <- waitForProcess ph
    return (ex, output)
