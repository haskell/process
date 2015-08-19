import Control.Exception
import System.Exit
import System.IO.Error
import System.Process

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

    putStrLn "Tests passed successfully"
