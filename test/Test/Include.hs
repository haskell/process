{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

#if defined(OS_STRING)
#define PATH_TYPE OsPath
#define STRING_TYPE OsString
#else
#define PATH_TYPE FilePath
#define STRING_TYPE String
#endif

-- Select the appropriate imports depending onf the version of the filepath
-- package.
#if MIN_VERSION_filepath(1,5,0)
#define OS_STRING_PACKAGE "os-string"
#define BYTESTRING_MODULE_PREFIX System.OsString
#else
#define OS_STRING_PACKAGE "filepath"
#define BYTESTRING_MODULE_PREFIX System.OsPath
#endif

module Test.STRING_TYPE (tests) where

import Control.Exception
import Control.Monad (guard, unless, void, when)
import System.Exit
import System.IO.Error
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Control.Concurrent
import Control.DeepSeq
import Data.Char (isDigit)
import Data.IORef
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import System.IO (hClose, hFlush, hGetContents, hPutStr)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as S8
import GHC.Conc.Sync (getUncaughtExceptionHandler, setUncaughtExceptionHandler)

-- OsString imports
#if defined(OS_STRING)
import Data.Maybe (fromJust)
import System.Directory.OsPath (getTemporaryDirectory, removeFile, exeExtension)
import System.IO (Handle)
import qualified System.IO (openBinaryTempFile)
import System.OsPath (OsPath, (<.>), osp)
import qualified System.OsPath as OsPath
import OS_STRING_PACKAGE System.OsString (OsString, osstr)
import qualified OS_STRING_PACKAGE System.OsString as OsString

import System.Process.CommunicationHandle.OsString
import System.Process.OsString
import System.Process.Internals.OsString (withForkWait, ignoreSigPipe)

-- String imports
#else
import System.Directory (getTemporaryDirectory, removeFile, exeExtension)
import System.FilePath ((<.>))
import System.IO (openBinaryTempFile)

import System.Process
import System.Process.CommunicationHandle
import System.Process.Internals (withForkWait, ignoreSigPipe)
#endif

#if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem ((<!>))
#endif

import Test.STRING_TYPE.Paths ( processInternalExes )

ifWindows :: IO () -> IO ()
ifWindows action
  | not isWindows = return ()
  | otherwise     = action

isWindows :: Bool
#if defined(mingw32_HOST_OS)
isWindows = True
#else
isWindows = False
#endif

tests :: IO ()
tests = do
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
    testCreateProcess
    testAddChdir
    testAddChdir2
    testCommunicationHandle False
#if defined(__IO_MANAGER_WINIO__)
    -- With WinIO, also run the test with the child process using WinIO
    testCommunicationHandle True
#endif
    putStrLn ">>> Tests passed successfully"

run :: String -> IO () -> IO ()
run label test = do
    putStrLn $ ">>> Running: " ++ label
    test

testDoesNotExist :: IO ()
testDoesNotExist = run "non-existent executable" $ do
    res <- handle (return . Left . isDoesNotExistError) $ do
        let
            cmd :: CreateProcess
            cmd = proc
#if defined(OS_STRING)
              [osp|definitelydoesnotexist|]
#else
              "definitelydoesnotexist"
#endif
              []
        (_, _, _, ph) <- createProcess cmd
            { close_fds = True
            }
        fmap Right $ waitForProcess ph
    case res of
        Left True -> return ()
        _ -> error $ show res

testModifiers :: IO ()
testModifiers = do
    let
        cmd :: CreateProcess
        cmd = proc
#if defined(OS_STRING)
            [osp|echo|] [[osstr|hello|], [osstr|world|]]
#else
            "echo" ["hello", "world"]
#endif

        test name modifier = run ("modifier " ++ name) $ do
            (_, _, _, ph) <- createProcess (modifier cmd)
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
    let
        cmd :: CreateProcess
        cmd = proc
#if defined(OS_STRING)
            [osp|./echo.bat|]
#else
            "./echo.bat"
#endif
            []

        cwd :: PATH_TYPE
        cwd =
#if defined(OS_STRING)
            [osp|subdir|]
#else
            "subdir"
#endif

    withCurrentDirectory "exes" $ do
      res1 <- readCreateProcess cmd ""
      unless ("parent" `isInfixOf` res1 && not ("child" `isInfixOf` res1)) $ error $
        "echo.bat with cwd failed: " ++ show res1

      res2 <- readCreateProcess cmd { cwd = Just cwd } ""
      unless ("child" `isInfixOf` res2 && not ("parent" `isInfixOf` res2)) $ error $
        "echo.bat with cwd failed: " ++ show res2

testBinaryHandles :: IO ()
testBinaryHandles = run "binary handles" $ do
    let
        template :: STRING_TYPE
        template =
#if defined(OS_STRING)
            [osstr|process-binary-test.bin|]
#else
            "process-binary-test.bin"
#endif

    tmpDir <- getTemporaryDirectory
    bracket
      (openBinaryTempFile tmpDir template)
      (\(fp, h) -> hClose h `finally` removeFile fp)
      $ \(fp, h) -> do
        let bs = S8.pack "hello\nthere\r\nworld\0"
        SBS.hPut h bs
        hClose h

        let
            cmd :: CreateProcess
            cmd = proc
#if defined(OS_STRING)
                [osp|cat|]
#else
                "cat"
#endif
                [fp]
        (Nothing, Just out, Nothing, ph) <- createProcess cmd
            { std_out = CreatePipe
            }
        res' <- SBS.hGetContents out
        hClose out
        ec <- waitForProcess ph
        unless (ec == ExitSuccess)
            $ error $ "Unexpected exit code " ++ show ec
        unless (bs == res')
            $ error $ "Unexpected result: " ++ show res'

testMultithreadedWait :: IO ()
testMultithreadedWait = run "multithreaded wait" $ do
    (_, _, _, p) <- createProcess (sleep 0.1)
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
    (_, _, _, p) <- createProcess (sleep 1.0)
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
      createProcess $ if isWindows
#if defined(OS_STRING)
        then (proc [osp|sh|] [[osstr|-c|], [osstr|z=$$; cat /proc/$z/winpid|]]) {std_out = CreatePipe}
        else (proc [osp|sh|] [[osstr|-c|], [osstr|echo $$|]]) {std_out = CreatePipe}
#else
        then (proc "sh" ["-c", "z=$$; cat /proc/$z/winpid"]) {std_out = CreatePipe}
        else (proc "sh" ["-c", "echo $$"]) {std_out = CreatePipe}
#endif
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
    output <- readProcess
#if defined(OS_STRING)
      [osp|echo|] [[osstr|hello|], [osstr|world|]] ""
#else
      "echo" ["hello", "world"] ""
#endif
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
        void $ readProcess
#if defined(OS_STRING)
            [osp|kill|] [[osstr|-INT|], showForTest pid] ""
#else
            "kill" ["-INT", show pid] ""
#endif

    -- collect unhandled exceptions in any threads (specifically
    -- the asynchronous 'waitForProcess' call from 'cleanupProcess')
    es <- collectExceptions $ do
        let cmd = (sleep 10) { delegate_ctlc = True }
        res <- try $ withCreateProcess cmd $ \_ _ _ p -> do
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
    (_, _, _, p) <- createProcess (sleep 0)
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
    run "terminate process, then wait twice (delegate_ctlc = False)" $ runTest sigTERM False
    run "terminate process, then wait twice (delegate_ctlc = True)" $ runTest sigTERM True
    run "interrupt process, then wait twice (delegate_ctlc = False)" $ runTest sigINT False
    run "interrupt process, then wait twice (delegate_ctlc = True)" $ runTest sigINT True
  where
    runTest :: STRING_TYPE -> Bool -> IO ()
    runTest sig delegate = do
        (_, _, _, p) <- createProcess (sleep 10) { delegate_ctlc = delegate }
        Just pid <- getPid p
        void $ readProcess
#if defined(OS_STRING)
            [osp|kill|] [[osstr|-|] <> sig, showForTest pid] ""
#else
            "kill" ["-" ++ sig, show pid] ""
#endif

        res <- try $ waitForProcess p
        checkFirst sig delegate res

        res' <- try $ waitForProcess p
        checkSecond sig delegate res'

    checkFirst :: STRING_TYPE -> Bool -> Either SomeException ExitCode -> IO ()
    checkFirst sig delegate res
        | sig == sigINT, delegate =
          case res of
            Left e -> case fromException e of
                Just UserInterrupt -> putStrLn "result ok"
                Nothing -> error $ "expected UserInterrupt, got  " ++ show e
            Right _ -> error $ "expected exception, got " ++ show res
        | otherwise =
          case res of
            Left e -> error $ "waitForProcess threw: " ++ show e
            Right ExitSuccess -> error "expected failure"
            _ -> putStrLn "result ok"

    checkSecond :: STRING_TYPE -> Bool -> Either SomeException ExitCode -> IO ()
    checkSecond sig delegate res
        | sig == sigINT, delegate = checkFirst sig False res
        | otherwise = checkFirst sig delegate res

    sigINT :: STRING_TYPE
#if defined(OS_STRING)
    sigINT = [osstr|INT|]
#else
    sigINT = "INT"
#endif

    sigTERM :: STRING_TYPE
#if defined(OS_STRING)
    sigTERM = [osstr|TERM|]
#else
    sigTERM = "TERM"
#endif

-- Test that createProcess doesn't segfault on Mac with a cwd of Nothing
testCreateProcess :: IO ()
testCreateProcess = run "createProcess with cwd = Nothing" $ do
    let env = CreateProcess
            { child_group = Nothing
            , child_user = Nothing
            , close_fds = False
#if defined(OS_STRING)
            , cmdspec = RawCommand [osp|env|] []
#else
            , cmdspec = RawCommand "env" []
#endif
            , create_group = True
            , create_new_console = False
            , cwd = Nothing
            , delegate_ctlc = False
            , detach_console = False
#if defined(OS_STRING)
            , env = Just [([osstr|PATH|], [osstr|/bin:/usr/bin|])]
#else
            , env = Just [("PATH", "/bin:/usr/bin")]
#endif
            , new_session = False
            , std_err = Inherit
            , std_in = Inherit
            , std_out = Inherit
            , use_process_jobs = False
            }
    (_, _, _, p) <- createProcess env
    res <- try $ waitForProcess p
    case res of
        Left e -> error $ "waitForProcess threw: " ++ show (e :: SomeException)
        Right ExitSuccess -> return ()
        Right exitCode -> error $ "unexpected exit code: " ++ show exitCode

testCommunicationHandle :: Bool -> IO ()
testCommunicationHandle childUsesWinIO = do
  parentUsesWinIO <-
    return False
#if defined(__IO_MANAGER_WINIO__)
      <!> return True
#endif
  putStr $ unlines
    [ "testCommunicationHandle {"
    , "parentUsesWinIO: " ++ show parentUsesWinIO
    ]
  -- Workaround for Cabal bug #9854 (cli-child executable not in PATH).
  let
      cliChild :: PATH_TYPE
      cliChild =
        case lookup "cli-child" processInternalExes of
          Just cliChildPath -> cliChildPath
#if defined(OS_STRING)
          Nothing -> [osp|cli-child|] <.> exeExtension
#else
          Nothing -> "cli-child" <.> exeExtension
#endif
  (ex, output) <-
    readCreateProcessWithExitCodeCommunicationHandle
      (\(chTheyRead, chTheyWrite) ->
        let
            args :: [STRING_TYPE]
            args = [showForTest chTheyRead, showForTest chTheyWrite]
                    ++ if childUsesWinIO
#if defined(OS_STRING)
                       then [[osstr|+RTS|], [osstr|--io-manager=native|], [osstr|-RTS|]]
#else
                       then ["+RTS", "--io-manager=native", "-RTS"]
#endif
                       else []
        in proc cliChild args)
      hGetContents
      (`hPutStr` "hello")
  case ex of
    ExitSuccess ->
      if output == "olleh123"
      then return ()
      else error $ "testCommunicationHandle: unexpected output " ++ show output
    ExitFailure {} ->
      error $ "testCommunicationHandle: child exited with exception " ++ show ex
  putStrLn "testCommunicationHandle }"

testAddChdir :: IO ()
testAddChdir = run "testAddChdir" $ handleIOErr $ do
  (_, _, _, commhand) <-
      runInteractiveProcess
#if defined(OS_STRING)
        [osp|true|] [] (Just [osp|/no/such/dir|]) Nothing
#else
        "true" [] (Just "/no/such/dir") Nothing
#endif
  exitCode <- waitForProcess commhand
  print exitCode
  where handleIOErr a = a `catchIOError` \e -> putStrLn ("Exc: " ++ show (ioeGetErrorType e))

testAddChdir2 :: IO ()
testAddChdir2 = run "testAddChdir2" $ handleIOErr $ do
  commhand <- runProcess
#if defined(OS_STRING)
                [osp|true|] [] (Just [osp|/no/such/dir|])
#else
                "true" [] (Just "/no/such/dir")
#endif
                Nothing Nothing Nothing Nothing
  exitCode <- waitForProcess commhand
  print exitCode
  where handleIOErr a = a `catchIOError` \e -> putStrLn ("Exc: " ++ show (ioeGetErrorType e))

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory new inner = do
  orig <- getCurrentDirectory
  bracket_
    (setCurrentDirectory new)
    (setCurrentDirectory orig)
    inner

catchThreadKilled :: IO a -> IO a -> IO a
catchThreadKilled f g = catchJust (\e -> guard (e == ThreadKilled)) f (\() -> g)

sleep :: (Fractional a, Show a) => a -> CreateProcess
sleep n =
  proc
#if defined(OS_STRING)
    [osp|sleep|]
#else
    "sleep"
#endif
    [showForTest n]

showForTest :: Show a => a -> STRING_TYPE
#if defined(OS_STRING)
showForTest = fromJust . OsString.encodeUtf . show
#else
showForTest = show
#endif

#if defined(OS_STRING)
-- Ideally we would depend on the `file-io` package and use the
-- `openBinaryTempFile` from that, but that doesn't build with GHC 8.6.
openBinaryTempFile :: OsPath -> OsString -> IO (OsPath, Handle)
openBinaryTempFile dirPath template = do
  decodedDirPath <- OsPath.decodeFS dirPath
  decodedTemplate <- OsPath.decodeFS template
  (filePath, h) <- System.IO.openBinaryTempFile decodedDirPath decodedTemplate
  encodedFilePath <- OsPath.encodeFS filePath
  pure (encodedFilePath, h)
#endif
