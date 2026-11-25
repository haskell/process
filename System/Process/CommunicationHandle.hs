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

import GHC.IO.Handle (Handle)

import System.Process.CommunicationHandle.Internal
import System.Process.Internals
  ( CreateProcess(..), createProcess_, ignoreSigPipe, withForkWait )
import System.Process
  ( cleanupProcess, waitForProcess )

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally, onException)
import GHC.IO.Handle (hClose)
import System.Exit (ExitCode)

import Control.DeepSeq (NFData, ($!!))

--------------------------------------------------------------------------------
-- Communication handles.

-- | Turn the 'CommunicationHandle' into a 'Handle' that can be read from
-- in the current process.
--
-- The returned 'Handle' does not have any finalizers attached to it;
-- use 'hClose' to close it.
--
-- @since 1.6.20.0
openCommunicationHandleRead :: CommunicationHandle -> IO Handle
openCommunicationHandleRead = useCommunicationHandle True

-- | Turn the 'CommunicationHandle' into a 'Handle' that can be written to
-- in the current process.
--
-- The returned 'Handle' does not have any finalizers attached to it;
-- use 'hClose' to close it.
--
-- @since 1.6.20.0
openCommunicationHandleWrite :: CommunicationHandle -> IO Handle
openCommunicationHandleWrite = useCommunicationHandle False

--------------------------------------------------------------------------------
-- Creating pipes.

-- | Create a pipe @(weRead,theyWrite)@ that the current process can read from,
-- and whose write end can be passed to a child process in order to receive data from it.
--
-- The returned 'Handle' does not have any finalizers attached to it;
-- use 'hClose' to close it.
--
-- See 'CommunicationHandle'.
--
-- @since 1.6.20.0
createWeReadTheyWritePipe
  :: IO (Handle, CommunicationHandle)
createWeReadTheyWritePipe =
  createCommunicationPipe id False
    -- safe choice: passAsyncHandleToChild = False, in case the child cannot
    -- deal with async I/O (see e.g. https://gitlab.haskell.org/ghc/ghc/-/issues/21610#note_431632)
    -- expert users can invoke createCommunicationPipe from
    -- System.Process.CommunicationHandle.Internals if they are sure that the
    -- child process they will communicate with supports async I/O on Windows

-- | Create a pipe @(theyRead,weWrite)@ that the current process can write to,
-- and whose read end can be passed to a child process in order to send data to it.
--
-- The returned 'Handle' does not have any finalizers attached to it;
-- use 'hClose' to close it.
--
-- See 'CommunicationHandle'.
--
-- @since 1.6.20.0
createTheyReadWeWritePipe
  :: IO (CommunicationHandle, Handle)
createTheyReadWeWritePipe =
  sw <$> createCommunicationPipe sw False
    -- safe choice: passAsyncHandleToChild = False, in case the child cannot
    -- deal with async I/O (see e.g. https://gitlab.haskell.org/ghc/ghc/-/issues/21610#note_431632)
    -- expert users can invoke createCommunicationPipe from
    -- System.Process.CommunicationHandle.Internals if they are sure that the
    -- child process they will communicate with supports async I/O on Windows
  where
    sw (a,b) = (b,a)

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
-- >   hRead  <- openCommunicationHandleRead  $ read chRead
-- >   hWrite <- openCommunicationHandleWrite $ read chWrite
-- >   input <- hGetContents hRead
-- >   hPut hWrite $ someFn input
-- >   hClose hWrite
--
-- @since 1.6.20.0
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
readCreateProcessWithExitCodeCommunicationHandle mkProg readAction writeAction =
  -- The following implementation parallels 'readCreateProcess'
  bracket spawn release $ \ ((hWeRead, hWeWrite), (_, _, _, ph)) -> do

    -- Run the read and write actions concurrently, as each may block until
    -- the child process makes progress on the other.
    outputVar <- newEmptyMVar
    let readOutput = do
          output <- readAction hWeRead
          putMVar outputVar $!! output
    withForkWait readOutput $ \ waitOut -> do
      ignoreSigPipe $ writeAction hWeWrite
      ignoreSigPipe $ hClose hWeWrite
      waitOut
      hClose hWeRead
    output <- takeMVar outputVar

    ex <- waitForProcess ph
    return (ex, output)
  where
    spawn = do
      (their_ends, our_ends) <- create_pipes
      p <- spawnWith their_ends `onException` close_our_ends our_ends
      return (our_ends, p)
    release (our_ends, p) = do
      cleanupProcess p
      close_our_ends our_ends
    spawnWith their_ends =
      createProcess_ "readCreateProcessWithExitCodeCommunicationHandle"
        (mkProg their_ends)
          `finally` close_their_ends their_ends

create_pipes :: IO ((CommunicationHandle, CommunicationHandle), (Handle, Handle))
create_pipes = do
  (chTheyRead, hWeWrite) <- createTheyReadWeWritePipe
  (hWeRead, chTheyWrite) <- createWeReadTheyWritePipe
    `onException` do { closeCommunicationHandle chTheyRead; hClose hWeWrite }
  return ((chTheyRead, chTheyWrite), (hWeRead, hWeWrite))

-- Close the parent's references to the 'CommunicationHandle's, whether or not
-- they have been inherited by a child process, to avoid keeping pipe ends open.
--
-- This must happen exactly once, as the underlying file descriptors may be
-- reused afterwards.
close_their_ends :: (CommunicationHandle, CommunicationHandle) -> IO ()
close_their_ends (chTheyRead, chTheyWrite) = do
  closeCommunicationHandle chTheyWrite
  closeCommunicationHandle chTheyRead

close_our_ends :: (Handle, Handle) -> IO ()
close_our_ends (hWeRead, hWeWrite) = do
  hClose hWeRead
  ignoreSigPipe $ hClose hWeWrite