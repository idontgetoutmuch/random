{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module System.Random.Monad
  (
    MonadRandom(..)
  ) where

import Control.Monad.IO.Class
import Data.Bits (unsafeShiftL, (.|.))
import Data.ByteString.Short.Internal (ShortByteString)
import Data.Word

import System.Random.Internal

-- | 'MonadRandom' is an interface to monadic pseudo-random number generators.
class Monad m => MonadRandom g s m | g m -> s where
  -- | Represents the state of the pseudo-random number generator for use with
  -- 'thawGen' and 'freezeGen'.
  --
  -- @since 1.2
  data Frozen g :: *
  {-# MINIMAL freezeGen,thawGen,(uniformWord32|uniformWord64) #-}

  -- | Restores the pseudo-random number generator from its 'Frozen'
  -- representation.
  --
  -- @since 1.2
  thawGen :: Frozen g -> m (g s)

  -- | Saves the state of the pseudo-random number generator to its 'Frozen'
  -- representation.
  --
  -- @since 1.2
  freezeGen :: g s -> m (Frozen g)

  -- | @uniformWord32R upperBound g@ generates a 'Word32' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2
  uniformWord32R :: Word32 -> g s -> m Word32
  uniformWord32R = unsignedBitmaskWithRejectionM uniformWord32

  -- | @uniformWord64R upperBound g@ generates a 'Word64' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2
  uniformWord64R :: Word64 -> g s -> m Word64
  uniformWord64R = unsignedBitmaskWithRejectionM uniformWord64

  -- | Generates a 'Word8' that is uniformly distributed over the entire 'Word8'
  -- range.
  --
  -- The default implementation extracts a 'Word8' from 'uniformWord32'.
  --
  -- @since 1.2
  uniformWord8 :: g s -> m Word8
  uniformWord8 = fmap fromIntegral . uniformWord32

  -- | Generates a 'Word16' that is uniformly distributed over the entire
  -- 'Word16' range.
  --
  -- The default implementation extracts a 'Word16' from 'uniformWord32'.
  --
  -- @since 1.2
  uniformWord16 :: g s -> m Word16
  uniformWord16 = fmap fromIntegral . uniformWord32

  -- | Generates a 'Word32' that is uniformly distributed over the entire
  -- 'Word32' range.
  --
  -- The default implementation extracts a 'Word32' from 'uniformWord64'.
  --
  -- @since 1.2
  uniformWord32 :: g s -> m Word32
  uniformWord32 = fmap fromIntegral . uniformWord64

  -- | Generates a 'Word64' that is uniformly distributed over the entire
  -- 'Word64' range.
  --
  -- The default implementation combines two 'Word32' from 'uniformWord32' into
  -- one 'Word64'.
  --
  -- @since 1.2
  uniformWord64 :: g s -> m Word64
  uniformWord64 g = do
    l32 <- uniformWord32 g
    h32 <- uniformWord32 g
    pure (unsafeShiftL (fromIntegral h32) 32 .|. fromIntegral l32)

  -- | @uniformShortByteString n g@ generates a 'ShortByteString' of length @n@
  -- filled with pseudo-random bytes.
  --
  -- @since 1.2
  uniformShortByteString :: Int -> g s -> m ShortByteString
  default uniformShortByteString :: MonadIO m => Int -> g s -> m ShortByteString
  uniformShortByteString n = genShortByteStringIO n . uniformWord64
  {-# INLINE uniformShortByteString #-}
