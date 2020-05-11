module Spec.Range
  ( symmetric
  , bounded
  , singleton
  , uniformRangeWithin
  , uniformRangeWithinExcluded
  ) where

import System.Random.Monad

symmetric :: (RandomGen g, UniformRange a, Eq a) => g -> (a, a) -> Bool
symmetric g (l, r) = fst (uniformR (l, r) g) == fst (uniformR (r, l) g)

bounded :: (RandomGen g, UniformRange a, Ord a) => g -> (a, a) -> Bool
bounded g (l, r) = bottom <= result && result <= top
  where
    bottom = min l r
    top = max l r
    result = fst (uniformR (l, r) g)

singleton :: (RandomGen g, UniformRange a, Eq a) => g -> a -> Bool
singleton g x = result == x
  where
    result = fst (uniformR (x, x) g)

uniformRangeWithin :: (RandomGen g, UniformRange a, Ord a) => g -> (a, a) -> Bool
uniformRangeWithin gen (l, r) =
  runStateGen_ gen $ \g ->
    (\result -> min l r <= result && result <= max l r) <$> uniformRM (l, r) g

uniformRangeWithinExcluded :: (RandomGen g, UniformRange a, Ord a) => g -> (a, a) -> Bool
uniformRangeWithinExcluded gen (l, r) =
  runStateGen_ gen $ \g ->
    (\result -> min l r <= result && (l == r || result < max l r)) <$> uniformRM (l, r) g
