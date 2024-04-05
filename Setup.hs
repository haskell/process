module Main (main) where

-- Cabal
import Distribution.Simple
  ( defaultMainWithHooks
  , autoconfUserHooks
  )

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainWithHooks autoconfUserHooks
