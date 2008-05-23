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

import System.Process
