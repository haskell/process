{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.Binary
-- Copyright   :  (c) The University of Glasgow 2004-2008
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires concurrency)
--
-- Operations for creating and interacting with sub-processes using
-- binary I/O. This module is intended to be imported qualified
-- alongside "System.Process", e.g.
--
-- > import System.Process
-- > import qualified System.Process.Binary as B
--
-----------------------------------------------------------------------------

module System.Process.Binary (  
  -- * Functions for common tasks
  readCreateProcess,
  readProcess,
  readCreateProcessWithExitCode,
  readProcessWithExitCode,  
  
  module Data.ByteString.Lazy,
  ) where

import qualified System.Process.Generic as G
import Data.ByteString.Lazy
import System.Process.Common (CreateProcess)
import System.Exit (ExitCode)

-- | Like 'System.Process.readProcess', but opens the standard input and output pipes
-- in binary mode.
  
readProcess :: FilePath -> [String] -> ByteString -> IO ByteString
readProcess = G.readProcess

-- | Like 'System.Process.readCreateProcess', but opens the standard input and output
-- pipes in binary mode.

readCreateProcess :: CreateProcess -> ByteString -> IO ByteString
readCreateProcess = G.readCreateProcess

-- | Like 'System.Process.readProcessWithExitCode', but opens the standard input, 
-- output, and error pipes in binary mode.

readProcessWithExitCode :: FilePath -> [String] -> ByteString -> IO (ExitCode,ByteString,ByteString)
readProcessWithExitCode = G.readProcessWithExitCode
    
-- | Like 'System.Process.readCreateProcessWithExitCode', but opens the standard input, 
-- output, and error pipes in binary mode.

readCreateProcessWithExitCode :: CreateProcess -> ByteString -> IO (ExitCode,ByteString,ByteString)
readCreateProcessWithExitCode = G.readCreateProcessWithExitCode
