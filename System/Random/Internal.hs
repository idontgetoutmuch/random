{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}

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
  , genShortByteStringIO
  , unsignedBitmaskWithRejectionM
  ) where

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.ByteString.Builder.Prim (word64LE)
import Data.ByteString.Builder.Prim.Internal (runF)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Word
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Exts
import GHC.IO (IO(..))

import qualified System.Random.SplitMix as SM

-- | The standard pseudo-random number generator.
newtype StdGen = StdGen { unStdGen :: SM.SMGen }
  deriving (Read, Show)

data MBA s = MBA (MutableByteArray# s)

-- | Efficiently generates a sequence of pseudo-random bytes in a platform
-- independent manner. The allocated memory is be pinned, so it is safe to use
-- with FFI calls.
--
-- @since 1.2
genShortByteStringIO :: MonadIO m => Int -> m Word64 -> m ShortByteString
genShortByteStringIO n0 gen64 = do
  let !n@(I# n#) = max 0 n0
      (n64, nrem64) = n `quotRem` 8
  MBA mba# <-
    liftIO $
    IO $ \s# ->
      case newPinnedByteArray# n# s# of
        (# s'#, mba# #) -> (# s'#, MBA mba# #)
  let go i ptr
        | i < n64 = do
          w64 <- gen64
          -- Writing 8 bytes at a time in a Little-endian order gives us
          -- platform portability
          liftIO $ runF word64LE w64 ptr
          go (i + 1) (ptr `plusPtr` 8)
        | otherwise = return ptr
  ptr <- go 0 (Ptr (byteArrayContents# (unsafeCoerce# mba#)))
  when (nrem64 > 0) $ do
    w64 <- gen64
    -- In order to not mess up the byte order we write generated Word64 into a
    -- temporary pointer and then copy only the missing bytes over to the array.
    -- It is tempting to simply generate as many bytes as we still need using
    -- smaller generators (eg. uniformWord8), but that would result in
    -- inconsistent tail when total length is slightly varied.
    liftIO $
      alloca $ \w64ptr -> do
        runF word64LE w64 w64ptr
        forM_ [0 .. nrem64 - 1] $ \i -> do
          w8 :: Word8 <- peekByteOff w64ptr i
          pokeByteOff ptr i w8
  liftIO $
    IO $ \s# ->
      case unsafeFreezeByteArray# mba# s# of
        (# s'#, ba# #) -> (# s'#, SBS ba# #)
{-# INLINE genShortByteStringIO #-}

-- | Uses the given generator function to generate an unsigned pseudo-random
-- number up to the given maximum (inclusive).
unsignedBitmaskWithRejectionM :: (Ord a, FiniteBits a, Num a, Monad m) => (g s -> m a) -> a -> g s -> m a
unsignedBitmaskWithRejectionM genUniformM range gen = go
  where
    mask = complement zeroBits `shiftR` countLeadingZeros (range .|. 1)
    go = do
      x <- genUniformM gen
      let x' = x .&. mask
      if x' > range
        then go
        else pure x'
{-# INLINE unsignedBitmaskWithRejectionM #-}
