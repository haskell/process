{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Cmd
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Executing an external command.
--
-- This module provides a simple interface for executing external commands.
-- For a more complex, but more powerful, interface, see the "System.Process"
-- module.
--
-----------------------------------------------------------------------------

-- later: {-# DEPRECATED "Use System.Process instead" #-}
module System.Cmd
    ( system,        -- :: String -> IO ExitCode
      rawSystem,     -- :: FilePath -> [String] -> IO ExitCode
    ) where

#ifndef __NHC__
import System.Process
#else
import System

rawSystem :: String -> [String] -> IO ExitCode
rawSystem cmd args = system (unwords (map translate (cmd:args)))

-- copied from System.Process (qv)
translate :: String -> String
translate str = '"' : snd (foldr escape (True,"\"") str)
  where escape '"'  (b,     str) = (True,  '\\' : '"'  : str)
        escape '\\' (True,  str) = (True,  '\\' : '\\' : str)
        escape '\\' (False, str) = (False, '\\' : str)
        escape c    (b,     str) = (False, c : str)

#endif

