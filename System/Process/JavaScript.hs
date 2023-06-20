{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

{-
  Child process support for JavaScript running on the node.js platform.

  Other platforms such as browsers will accept the JavaScript code, but all
  operations will result in unsupported operation exceptions.
 -}

#include "HsProcessConfig.h"

module System.Process.JavaScript
    ( mkProcessHandle
    , translateInternal
    , createProcess_Internal
    , withCEnvironment
    , closePHANDLE
    , startDelegateControlC
    , endDelegateControlC
    , stopDelegateControlC
    , isDefaultSignal
    , ignoreSignal
    , defaultSignal
    , createPipeInternal
    , createPipeInternalFd
    , interruptProcessGroupOfInternal
    , getProcessId
    , getCurrentProcessId
    ) where

import Control.Concurrent.MVar
import Control.Exception (throwIO)

import Data.Char (isAlphaNum)

import System.Exit
import System.IO
import System.IO.Error
import qualified System.Posix.Internals as Posix

import Foreign.C
import Foreign.Marshal
import Foreign.Ptr

import GHC.IO.Handle.FD (mkHandleFromFD)
import GHC.IO.Device (IODeviceType(..))
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.Exception
import qualified GHC.IO.FD as FD

import GHC.JS.Prim

import System.Process.Common hiding (mb_delegate_ctlc, mbPipe)

mkProcessHandle :: JSVal -> Bool -> IO ProcessHandle
mkProcessHandle p mb_delegate_ctlc = do
  m <- newMVar (OpenHandle p)
  ml <- newMVar ()
  return (ProcessHandle m mb_delegate_ctlc ml)

closePHANDLE :: JSVal -> IO ()
closePHANDLE _ = return ()

getProcessId :: PHANDLE -> IO Int
getProcessId ph =
  throwErrnoIfMinus1 "getProcessId" (js_getProcessId ph)

getCurrentProcessId :: IO Int
getCurrentProcessId =
  throwErrnoIfMinus1 "getCurrentProcessId" js_getCurrentProcessId

startDelegateControlC :: IO ()
startDelegateControlC =
  throwErrnoIfMinus1_ "startDelegateControlC" js_startDelegateControlC

stopDelegateControlC :: IO ()
stopDelegateControlC =
  throwErrnoIfMinus1_ "stopDelegateControlC" js_stopDelegateControlC

endDelegateControlC :: ExitCode -> IO ()
endDelegateControlC (ExitFailure (-2)) = throwIO UserInterrupt -- SIGINT
endDelegateControlC _                  = pure ()

ignoreSignal, defaultSignal :: CLong
ignoreSignal  = CONST_SIG_IGN
defaultSignal = CONST_SIG_DFL

isDefaultSignal :: CLong -> Bool
isDefaultSignal = (== defaultSignal)

interruptProcessGroupOfInternal
    :: ProcessHandle    -- ^ A process in the process group
    -> IO ()
interruptProcessGroupOfInternal ph =
      withProcessHandle ph $ \p_ -> do
        case p_ of
            OpenExtHandle{} -> return ()
            ClosedHandle  _ -> return ()
            OpenHandle    h ->
                throwErrnoIfMinus1_ "interruptProcessGroupOfInternal"
                                    (js_interruptProcessGroupOf h)

translateInternal :: String -> String
translateInternal "" = "''"
translateInternal str
   -- goodChar is a pessimistic predicate, such that if an argument is
   -- non-empty and only contains goodChars, then there is no need to
   -- do any quoting or escaping
 | all goodChar str = str
 | otherwise        = '\'' : foldr escape "'" str
  where escape '\'' = showString "'\\''"
        escape c    = showChar c
        goodChar c = isAlphaNum c || c `elem` "-_.,/"

-- node.js does not appear to have any built-in facilities
-- for creating pipes, so we leave this as an unsupported operation
-- for now
createPipeInternal :: IO (Handle, Handle)
createPipeInternal = ioError
  (ioeSetLocation unsupportedOperation "createPipeInternal")

createPipeInternalFd :: IO (Posix.FD, Posix.FD)
createPipeInternalFd = ioError
  (ioeSetLocation unsupportedOperation "createPipeInternalFd")

withCEnvironment :: [(String,String)] -> (Ptr CString  -> IO a) -> IO a
withCEnvironment envir act =
  let env' = map (\(name, val) -> name ++ ('=':val)) envir
  in withMany withCString env' (\pEnv -> withArray0 nullPtr pEnv act)

commandToProcess :: CmdSpec -> IO (FilePath, [String])
commandToProcess cmd =
  case cmd of
    ShellCommand xs   -> c2p (toJSString xs) jsNull
    RawCommand c args -> c2p (toJSString c) =<< toJSStrings args
  where
    c2p c as = do
      r <- throwErrnoIfJSNull "commandToProcess" (js_commandToProcess c as)
      fromJSStrings r >>= \case
        (x:xs) -> pure (x,xs)
        _      -> error "commandToProcess: empty list"

-- -----------------------------------------------------------------------------
-- JavaScript nodejs runProcess with signal handling in the child

createProcess_Internal
  :: String
       -- ^ Function name (for error messages).
       --
       --   This can be any 'String', but will typically be the name of the caller.
       --   E.g., 'spawnProcess' passes @"spawnProcess"@ here when calling
       --   'createProcess_'.
  -> CreateProcess
  -> IO ProcRetHandles
createProcess_Internal fun CreateProcess{ cmdspec = cmdsp,
                                  cwd = mb_cwd,
                                  env = mb_env,
                                  std_in = mb_stdin,
                                  std_out = mb_stdout,
                                  std_err = mb_stderr,
                                  close_fds = mb_close_fds,
                                  create_group = mb_create_group,
                                  delegate_ctlc = mb_delegate_ctlc,
                                  new_session = mb_new_session,
                                  child_user = mb_child_user,
                                  child_group = mb_child_group }
 = do
  (cmd, args) <- commandToProcess cmdsp
  withFilePathException cmd $ do
     fdin  <- mbFd fun fd_stdin  mb_stdin
     fdout <- mbFd fun fd_stdout mb_stdout
     fderr <- mbFd fun fd_stderr mb_stderr
     env'  <- maybe (pure jsNull)
                    (toJSStrings . concatMap (\(x,y) -> [x,y]))
                    mb_env

     let cwd' = maybe jsNull toJSString mb_cwd
     let c1 = toJSString cmd
     c2 <- case args of
               [] -> return jsNull
               _  -> toJSStrings args

     r <- throwErrnoIfJSNull fun $
             js_runInteractiveProcess c1
                                      c2
                                      cwd'
                                      env'
                                      fdin
                                      fdout
                                      fderr
                                      mb_close_fds
                                      mb_create_group
                                      mb_delegate_ctlc
                                      mb_new_session
                                      (maybe (-1) fromIntegral mb_child_group)
                                      (maybe (-1) fromIntegral mb_child_user)

     fdin_r:fdout_r:fderr_r:_ <-
         map (stdFD . fromIntegral) <$> (fromJSInts =<< getProp r "fds")

     hndStdInput  <- mbPipe mb_stdin  fdin_r  WriteMode
     hndStdOutput <- mbPipe mb_stdout fdout_r ReadMode
     hndStdError  <- mbPipe mb_stderr fderr_r ReadMode

     ph <- mkProcessHandle r mb_delegate_ctlc
     return $ ProcRetHandles { hStdInput = hndStdInput
                             , hStdOutput = hndStdOutput
                             , hStdError = hndStdError
                             , procHandle = ph
                             }

mbPipe :: StdStream -> FD.FD -> IOMode -> IO (Maybe Handle)
mbPipe CreatePipe fd mode = do
  enc <- getLocaleEncoding
  fmap Just (mkHandleFromFD fd
                            Stream
                            ("fd: " ++ show fd)
                            mode
                            False {-is_socket-}
                            (Just enc))
mbPipe _ _ _ = do
  return Nothing

stdFD :: CInt -> FD.FD
stdFD fd = FD.FD { FD.fdFD = fd
                 , FD.fdIsNonBlocking = 0
                 }

-- -----------------------------------------------------------------------------
-- Some helpers for dealing with JavaScript values

-- JavaScript value type synonyms, for readability
type JSArray  = JSVal
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

-- -----------------------------------------------------------------------------
-- Foreign imports from process.js

-- run an interactive process. Note that this foreign import is asynchronous
-- (interruptible) since it waits until the process has spawned (or an error
-- has occurred.
--
-- this should only be a short time, so it should be safe to call this from
-- an uninterruptible mask.

foreign import javascript interruptible "h$process_runInteractiveProcess"
  js_runInteractiveProcess
        :: JSString     -- ^ command or program
        -> JSArray      -- ^ arguments, null if it's a raw command
        -> JSString     -- ^ working dir, null for current
        -> JSArray      -- ^ environment, null for existing
        -> CInt         -- ^ stdin fd
        -> CInt         -- ^ stdout fd
        -> CInt         -- ^ stderr fd
        -> Bool         -- ^ close file descriptors in child (currently unsupported)
        -> Bool         -- ^ create a new process group
        -> Bool         -- ^ delegate ctrl-c
        -> Bool         -- ^ create a new session
        -> Int          -- ^ set child GID (-1 for unchanged)
        -> Int          -- ^ set child UID (-1 for unchanged)
        -> IO JSVal     -- ^ process handle (null if an error occurred)

foreign import javascript safe "h$process_commandToProcess"
  js_commandToProcess
        :: JSString
        -> JSArray
        -> IO JSArray

foreign import javascript unsafe "h$process_interruptProcessGroupOf"
  js_interruptProcessGroupOf
        :: PHANDLE
        -> IO Int

foreign import javascript unsafe "h$process_startDelegateControlC"
  js_startDelegateControlC
        :: IO Int

foreign import javascript unsafe "h$process_stopDelegateControlC"
  js_stopDelegateControlC
        :: IO Int

foreign import javascript unsafe "h$process_getCurrentProcessId"
  js_getCurrentProcessId
        :: IO Int

foreign import javascript unsafe "h$process_getProcessId"
  js_getProcessId
        :: PHANDLE
        -> IO Int
