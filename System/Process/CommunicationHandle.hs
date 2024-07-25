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
  ( CreateProcess(..), ignoreSigPipe, withForkWait )
import System.Process
  ( withCreateProcess, waitForProcess )

import GHC.IO (evaluate)
import GHC.IO.Handle (hClose)
import System.Exit (ExitCode)

import Control.DeepSeq (NFData, rnf)

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
