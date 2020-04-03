{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      :  System.Random
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
module System.Random.Reader
  ( HasGen(..)
  , uniformEnvM
  , uniformEnvRM
  , module System.Random
  ) where

import System.Random

import Control.Monad.Reader

class HasGen env g | env -> g where
  getGen :: env -> g

instance HasGen (PureGen g) (PureGen g) where
  getGen = id

instance HasGen (MutGen s g) (MutGen s g) where
  getGen = id

instance HasGen (PrimGen s g) (PrimGen s g) where
  getGen = id

-- | Same as `uniform`, but get the generator from the reader environment
uniformEnvM :: (HasGen env g, MonadReader env m, MonadRandom g m, Uniform a) => m a
uniformEnvM = uniform . getGen =<< ask

-- | Same as `uniformR`, but get the generator from the reader environment
uniformEnvRM :: (HasGen env g, MonadReader env m, MonadRandom g m, UniformRange a) => (a, a) -> m a
uniformEnvRM r = uniformR r . getGen =<< ask
