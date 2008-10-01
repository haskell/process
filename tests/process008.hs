{-# OPTIONS -cpp #-}
import System.IO
import System.Cmd

test = rawSystem "printf" ["testing\n"]

main = test >> test >> return ()
