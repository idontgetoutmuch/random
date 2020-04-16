{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :  System.Random.Internal
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
--
-- Exposes internal definitions

module System.Random.Internal
  (
    StdGen(..)
  ) where

import qualified System.Random.SplitMix as SM

-- | The standard pseudo-random number generator.
newtype StdGen = StdGen { unStdGen :: SM.SMGen }
  deriving (Read, Show)
