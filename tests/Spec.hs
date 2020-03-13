{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Spec.Bitmask as Bitmask
import qualified Spec.Bitmask as Range
import System.Random
import Data.Word (Word32, Word64)
import Test.Tasty
import Test.Tasty.SmallCheck as SC

main :: IO ()
main = defaultMain $ testGroup "Spec"
    [ bitmaskSpecWord32, bitmaskSpecWord64
    , rangeSpecWord32, rangeSpecInt
    ]

bitmaskSpecWord32 :: TestTree
bitmaskSpecWord32 = testGroup "bitmaskWithRejection (Word32)"
    [ SC.testProperty "symmetric" $ seeded $ Bitmask.symmetric @StdGen @Word32
    , SC.testProperty "bounded" $ seeded $ Bitmask.bounded @StdGen @Word32
    , SC.testProperty "singleton" $ seeded $ Bitmask.singleton @StdGen @Word32
    ]

bitmaskSpecWord64 :: TestTree
bitmaskSpecWord64 = testGroup "bitmaskWithRejection (Word64)"
    [ SC.testProperty "symmetric" $ seeded $ Bitmask.symmetric @StdGen @Word64
    , SC.testProperty "bounded" $ seeded $ Bitmask.bounded @StdGen @Word64
    , SC.testProperty "singleton" $ seeded $ Bitmask.singleton @StdGen @Word64
    ]

rangeSpecWord32 :: TestTree
rangeSpecWord32 = testGroup "uniformR (Word32)"
    [ SC.testProperty "(Word32) symmetric" $ seeded $ Range.symmetric @StdGen @Word32
    , SC.testProperty "(Word32) bounded" $ seeded $ Range.bounded @StdGen @Word32
    , SC.testProperty "(Word32) singleton" $ seeded $ Range.singleton @StdGen @Word32
    ]

rangeSpecInt :: TestTree
rangeSpecInt = testGroup "uniformR (Int)"
    [ SC.testProperty "(Int) symmetric" $ seeded $ Range.symmetric @StdGen @Int
    -- FIXME Make this pass
    -- , SC.testProperty "(Int) bounded" $ seeded $ Range.bounded @StdGen @Int
    , SC.testProperty "(Int) singleton" $ seeded $ Range.singleton @StdGen @Int
    ]

-- | Create a StdGen instance from an Int and pass it to the given function.
seeded :: (StdGen -> a) -> Int -> a
seeded f = f . mkStdGen
