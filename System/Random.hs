-- |
-- Module      :  System.Random
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
--
-- This modules exports a subset of this library's functionality for backwards
-- compatibility with version 1.1.
--
-- __See "System.Random.Monad" for the full documentation.__
module System.Random
  (
  -- * Pure pseudo-random number generator interface
    RandomGen(..)

  -- ** Standard pseudo-random number generator
  , StdGen
  , mkStdGen

  -- ** Global standard pseudo-random number generator
  , getStdRandom
  , getStdGen
  , setStdGen
  , newStdGen

  -- * Pseudo-random values of various types
  , Random(..)
  ) where

import System.Random.Internal
