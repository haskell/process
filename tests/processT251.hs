import Control.Exception
import GHC.IO.Exception
import System.Environment
import System.Exit
import System.Process

main :: IO ()
main = do
    args <- getArgs
    case args of
      []         -> parent
      ["child"]  -> child
      ["child2"] -> child2
      _          -> fail "unknown mode"

parent :: IO ()
parent = do
    putStrLn "parent start"
    (_, _, _, phdl) <- createProcess $ (proc "./processT251" ["child"]) { std_in = NoStream }
    ExitSuccess <- waitForProcess phdl
    putStrLn "parent done"

child :: IO ()
child = do
    putStrLn "child start"
    (_, _, _, phdl) <- createProcess $ (proc "./processT251" ["child2"]) { std_in = NoStream }
    ExitSuccess <- waitForProcess phdl
    putStrLn "child done"

child2 :: IO ()
child2 = do
    putStrLn "child2 start"
    Left (IOError {ioe_type=InvalidArgument}) <-
        try $ getContents >>= print
    putStrLn "child2 done"

