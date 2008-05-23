
import System.Process
import System.IO
import System.Posix
import System.Exit

tmpfile = "process007.tmp"

main = do
  writeFile tmpfile "You bad pie-rats!\n"
  fd <- handleToFd =<< openFile tmpfile ReadMode
  system ("cat 0<&" ++ show fd)
  -- or this, but maybe less portable?
  --    rawSystem "cat" ["/dev/fd/" ++ show fd]
  closeFd fd

  fd <- handleToFd =<< openFile tmpfile ReadMode
  nul <- openFile "/dev/null" WriteMode
  (_,_,_,p) <- createProcess (shell ("cat 0<&" ++ show fd))
                               { close_fds = True,
                                 std_err = UseHandle nul }
  e <- waitForProcess p
  case e of
       ExitSuccess -> putStrLn "eek!"
       _ -> putStrLn "failed, as expected"
  closeFd fd
