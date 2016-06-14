import Control.Exception
import Control.Monad (unless)
import System.Exit
import System.IO.Error
import System.Process
import System.IO (hClose, openBinaryTempFile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import System.Directory (getTemporaryDirectory, removeFile)

main :: IO ()
main = do
    res <- handle (return . Left . isDoesNotExistError) $ do
        (_, _, _, ph) <- createProcess (proc "definitelydoesnotexist" [])
            { close_fds = True
            }
        fmap Right $ waitForProcess ph
    case res of
        Left True -> return ()
        _ -> error $ show res

    let test name modifier = do
            putStrLn $ "Running test: " ++ name
            (_, _, _, ph) <- createProcess
                $ modifier $ proc "echo" ["hello", "world"]
            ec <- waitForProcess ph
            if ec == ExitSuccess
                then putStrLn $ "Success running: " ++ name
                else error $ "echo returned: " ++ show ec

    test "detach_console" $ \cp -> cp { detach_console = True }
    test "create_new_console" $ \cp -> cp { create_new_console = True }
    test "new_session" $ \cp -> cp { new_session = True }

    putStrLn "Binary handles"
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

    putStrLn "Tests passed successfully"
