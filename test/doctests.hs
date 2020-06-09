{-# LANGUAGE CPP #-}
module Main where

#if __GLASGOW_HASKELL__ >= 800

import Build_doctests (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import Test.DocTest (doctest)

main :: IO ()
main = do
    traverse_ putStrLn args
    doctest args
  where
    args = flags ++ pkgs ++ module_sources

#else

-- TODO: fix doctest support
main :: IO ()
main = putStrLn "\nDoctests are not supported for older ghc version\n"

#endif
