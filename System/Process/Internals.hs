{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.Internals
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- __Note:__ This module exports internal implementation details that may
-- change anytime.  If you want a more stable API, use "System.Process"
-- instead.
--
-----------------------------------------------------------------------------

module System.Process.Internals
  ( module System.Process.Internals.String
  ) where

import System.Process.Internals.String
