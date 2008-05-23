{-# OPTIONS -cpp #-}
import System.IO
import System.Cmd

test = rawSystem "echo" ["testing"]

main = test >> test >> return ()
