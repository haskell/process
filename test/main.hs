{-# LANGUAGE CPP #-}
import Control.Exception
import Control.Monad (guard, unless, void)
import System.Exit
import System.IO.Error
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Process
import Control.Concurrent
import Data.Char (isDigit)
import Data.IORef
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import System.IO (hClose, openBinaryTempFile, hGetContents)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import System.Directory (getTemporaryDirectory, removeFile)
import GHC.Conc.Sync (getUncaughtExceptionHandler, setUncaughtExceptionHandler)

ifWindows :: IO () -> IO ()
ifWindows action
  | not isWindows = return ()
  | otherwise     = action

isWindows :: Bool
#if WINDOWS
isWindows = True
#else
isWindows = False
#endif

main :: IO ()
main = do
    testDoesNotExist
    testModifiers
    testSubdirectories
    testBinaryHandles
    testMultithreadedWait
    testInterruptMaskedWait
    testGetPid
    testReadProcess
    testInterruptWith
    testDoubleWait
    testKillDoubleWait
    putStrLn ">>> Tests passed successfully"

run :: String -> IO () -> IO ()
run label test = do
    putStrLn $ ">>> Running: " ++ label
    test

testDoesNotExist :: IO ()
testDoesNotExist = run "non-existent executable" $ do
    res <- handle (return . Left . isDoesNotExistError) $ do
        (_, _, _, ph) <- createProcess (proc "definitelydoesnotexist" [])
            { close_fds = True
            }
        fmap Right $ waitForProcess ph
    case res of
        Left True -> return ()
        _ -> error $ show res

testModifiers :: IO ()
testModifiers = do
    let test name modifier = run ("modifier " ++ name) $ do
            (_, _, _, ph) <- createProcess
                $ modifier $ proc "echo" ["hello", "world"]
            ec <- waitForProcess ph
            unless (ec == ExitSuccess)
                $ error $ "echo returned: " ++ show ec

    test "vanilla" id

    -- FIXME need to debug this in the future on Windows
    unless isWindows $ test "detach_console" $ \cp -> cp { detach_console = True }

    test "create_new_console" $ \cp -> cp { create_new_console = True }
    test "new_session" $ \cp -> cp { new_session = True }

testSubdirectories :: IO ()
testSubdirectories = ifWindows $ run "subdirectories" $ do
    withCurrentDirectory "exes" $ do
      res1 <- readCreateProcess (proc "./echo.bat" []) ""
      unless ("parent" `isInfixOf` res1 && not ("child" `isInfixOf` res1)) $ error $
        "echo.bat with cwd failed: " ++ show res1

      res2 <- readCreateProcess (proc "./echo.bat" []) { cwd = Just "subdir" } ""
      unless ("child" `isInfixOf` res2 && not ("parent" `isInfixOf` res2)) $ error $
        "echo.bat with cwd failed: " ++ show res2

testBinaryHandles :: IO ()
testBinaryHandles = run "binary handles" $ do
    tmpDir <- getTemporaryDirectory
    bracket
      (openBinaryTempFile tmpDir "process-binary-test.bin")
      (\(fp, h) -> hClose h `finally` removeFile fp)
      $ \(fp, h) -> do
        let bs = S8.pack "hello\nthere\r\nworld\0"
        S.hPut h bs
        hClose h

        (Nothing, Just out, Nothing, ph) <- createProcess (proc "cat" [fp])
            { std_out = CreatePipe
            }
        res' <- S.hGetContents out
        hClose out
        ec <- waitForProcess ph
        unless (ec == ExitSuccess)
            $ error $ "Unexpected exit code " ++ show ec
        unless (bs == res')
            $ error $ "Unexpected result: " ++ show res'

testMultithreadedWait :: IO ()
testMultithreadedWait = run "multithreaded wait" $ do
    (_, _, _, p) <- createProcess (proc "sleep" ["0.1"])
    me1 <- newEmptyMVar
    _ <- forkIO . void $ waitForProcess p >>= putMVar me1
    -- check for race / deadlock between waitForProcess and getProcessExitCode
    e3 <- getProcessExitCode p
    e2 <- waitForProcess p
    e1 <- readMVar me1
    unless (isNothing e3)
          $ error $ "unexpected exit " ++ show e3
    unless (e1 == ExitSuccess && e2 == ExitSuccess)
          $ error "sleep exited with non-zero exit code!"

testInterruptMaskedWait :: IO ()
testInterruptMaskedWait = run "interrupt masked wait" $ do
    (_, _, _, p) <- createProcess (proc "sleep" ["1.0"])
    mec <- newEmptyMVar
    tid <- mask_ . forkIO $
        (waitForProcess p >>= putMVar mec . Just)
            `catchThreadKilled` putMVar mec Nothing
    killThread tid
    eec <- takeMVar mec
    case eec of
      Nothing -> return ()
      Just ec ->
        if isWindows
          then putStrLn "FIXME ignoring known failure on Windows"
          else error $ "waitForProcess not interrupted: sleep exited with " ++ show ec

testGetPid :: IO ()
testGetPid = run "getPid" $ do
    (_, Just out, _, p) <-
      if isWindows
        then createProcess $ (proc "sh" ["-c", "z=$$; cat /proc/$z/winpid"]) {std_out = CreatePipe}
        else createProcess $ (proc "sh" ["-c", "echo $$"]) {std_out = CreatePipe}
    pid <- getPid p
    line <- hGetContents out
    putStrLn $ " queried PID: " ++ show pid
    putStrLn $ " PID reported by stdout: " ++ show line
    _ <- waitForProcess p
    hClose out
    let numStdoutPid = read (takeWhile isDigit line) :: Pid
    unless (Just numStdoutPid == pid) $
      if isWindows
        then putStrLn "FIXME ignoring known failure on Windows"
        else error "subprocess reported unexpected PID"

testReadProcess :: IO ()
testReadProcess = run "readProcess" $ do
    output <- readProcess "echo" ["hello", "world"] ""
    unless (output == "hello world\n") $
        error $ "unexpected output, got: " ++ output

-- | Test that withCreateProcess doesn't throw exceptions besides
-- the expected UserInterrupt when the child process is interrupted
-- by Ctrl-C.
testInterruptWith :: IO ()
testInterruptWith = unless isWindows $ run "interrupt withCreateProcess" $ do
    mpid <- newEmptyMVar
    forkIO $ do
        pid <- takeMVar mpid
        void $ readProcess "kill" ["-INT", show pid] ""

    -- collect unhandled exceptions in any threads (specifically
    -- the asynchronous 'waitForProcess' call from 'cleanupProcess')
    es <- collectExceptions $ do
        let sleep = (proc "sleep" ["10"]) { delegate_ctlc = True }
        res <- try $ withCreateProcess sleep $ \_ _ _ p -> do
            Just pid <- getPid p
            putMVar mpid pid
            waitForProcess p
        unless (res == Left UserInterrupt) $
            error $ "expected UserInterrupt, got " ++ show res

    unless (null es) $
        error $ "uncaught exceptions: " ++ show es

  where
    collectExceptions action = do
        oldHandler <- getUncaughtExceptionHandler
        flip finally (setUncaughtExceptionHandler oldHandler) $ do
            exceptions <- newIORef ([] :: [SomeException])
            setUncaughtExceptionHandler (\e -> atomicModifyIORef exceptions $ \es -> (e:es, ()))
            action
            threadDelay 1000 -- give some time for threads to finish
            readIORef exceptions

-- Test that we can wait without exception twice, if the process exited on its own.
testDoubleWait :: IO ()
testDoubleWait = run "run process, then wait twice" $ do
    let sleep = (proc "sleep" ["0"])
    (_, _, _, p) <- createProcess sleep
    res <- try $ waitForProcess p
    case res of
        Left e -> error $ "waitForProcess threw: " ++ show (e :: SomeException)
        Right ExitSuccess -> return ()
        Right exitCode -> error $ "unexpected exit code: " ++ show exitCode

    res2 <- try $ waitForProcess p
    case res2 of
        Left e -> error $ "second waitForProcess threw: " ++ show (e :: SomeException)
        Right ExitSuccess -> return ()
        Right exitCode -> error $ "unexpected exit code: " ++ show exitCode

-- Test that we can wait without exception twice, if the process was killed.
testKillDoubleWait :: IO ()
testKillDoubleWait = unless isWindows $ do
    run "terminate process, then wait twice (delegate_ctlc = False)" $ runTest "TERM" False
    run "terminate process, then wait twice (delegate_ctlc = True)" $ runTest "TERM" True
    run "interrupt process, then wait twice (delegate_ctlc = False)" $ runTest "INT" False
    run "interrupt process, then wait twice (delegate_ctlc = True)" $ runTest "INT" True
  where
    runTest sig delegate = do
        let sleep = (proc "sleep" ["10"])
        (_, _, _, p) <- createProcess sleep { delegate_ctlc = delegate }
        Just pid <- getPid p
        void $ readProcess "kill" ["-" ++ sig, show pid] ""

        res <- try $ waitForProcess p
        checkFirst sig delegate res

        res' <- try $ waitForProcess p
        checkSecond sig delegate res'

    checkFirst :: String -> Bool -> Either SomeException ExitCode -> IO ()
    checkFirst sig delegate res = case (sig, delegate) of
        ("INT", True) -> case res of
            Left e -> case fromException e of
                Just UserInterrupt -> putStrLn "result ok"
                Nothing -> error $ "expected UserInterrupt, got  " ++ show e
            Right _ -> error $ "expected exception, got " ++ show res
        _ -> case res of
            Left e -> error $ "waitForProcess threw: " ++ show e
            Right ExitSuccess -> error "expected failure"
            _ -> putStrLn "result ok"

    checkSecond :: String -> Bool -> Either SomeException ExitCode -> IO ()
    checkSecond sig delegate res = case (sig, delegate) of
        ("INT", True) -> checkFirst "INT" False res
        _ -> checkFirst sig delegate res

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory new inner = do
  orig <- getCurrentDirectory
  bracket_
    (setCurrentDirectory new)
    (setCurrentDirectory orig)
    inner

catchThreadKilled :: IO a -> IO a -> IO a
catchThreadKilled f g = catchJust (\e -> guard (e == ThreadKilled)) f (\() -> g)
