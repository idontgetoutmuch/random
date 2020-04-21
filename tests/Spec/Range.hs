{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Spec.Range
  ( symmetric
  , bounded
  , singleton
  , uniformRangeWithin
  , uniformRangeWithinExcluded
  ) where

import System.Random.Monad

symmetric :: (RandomGen g, Random a, Eq a) => g -> (a, a) -> Bool
symmetric g (l, r) = fst (randomR (l, r) g) == fst (randomR (r, l) g)

bounded :: (RandomGen g, Random a, Ord a) => g -> (a, a) -> Bool
bounded g (l, r) = bottom <= result && result <= top
  where
    bottom = min l r
    top = max l r
    result = fst (randomR (l, r) g)

singleton :: (RandomGen g, Random a, Eq a) => g -> a -> Bool
singleton g x = result == x
  where
    result = fst (randomR (x, x) g)

uniformRangeWithin ::
     (RandomGen g, UniformRange 'Inclusive 'Inclusive a, Ord a)
  => g
  -> (Inc a, Inc a)
  -> Bool
uniformRangeWithin gen r@(Inc l, Inc u) =
  runGenState_ gen $ \g ->
    (\result -> min l u <= result && result <= max l u) <$> uniformRM r g

uniformRangeWithinExcluded ::
     (RandomGen g, UniformRange 'Inclusive 'Exclusive a, Ord a)
  => g
  -> (Inc a, Exc a)
  -> Bool
uniformRangeWithinExcluded gen r@(Inc l, Exc u) =
  runGenState_ gen $ \g ->
    (\result -> min l u <= result && (l == u || result < max l u)) <$> uniformRM r g
