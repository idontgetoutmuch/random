{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

#include "MachDeps.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Random
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This library deals with the common task of pseudo-random number
-- generation. The library makes it possible to generate repeatable
-- results, by starting with a specified initial random number generator,
-- or to get different results on each run by using the system-initialised
-- generator or by supplying a seed from some other source.
--
-- The library is split into two layers:
--
-- * A core /random number generator/ provides a supply of bits.
--   The class 'RandomGen' provides a common interface to such generators.
--   The library provides one instance of 'RandomGen', the abstract
--   data type 'StdGen'.  Programmers may, of course, supply their own
--   instances of 'RandomGen'.
--
-- * The class 'Random' provides a way to extract values of a particular
--   type from a random number generator.  For example, the 'Float'
--   instance of 'Random' allows one to generate random values of type
--   'Float'.
--
-- This implementation uses the SplitMix algorithm [1].
--
-- [/Example for RNG Implementors:/]
--
-- Suppose you want to use a [permuted congruential
-- generator](https://en.wikipedia.org/wiki/Permuted_congruential_generator)
-- as the source of entropy (FIXME: is that the correct
-- terminology). You can make it an instance of `RandomGen`:
--
-- >>> data PCGen = PCGen !Word64 !Word64
--
-- >>> :{
-- let stepGen :: PCGen -> (Word32, PCGen)
--     stepGen (PCGen state inc) = let
--       newState = state * 6364136223846793005 + (inc .|. 1)
--       xorShifted = fromIntegral (((state `shiftR` 18) `xor` state) `shiftR` 27) :: Word32
--       rot = fromIntegral (state `shiftR` 59) :: Word32
--       out = (xorShifted `shiftR` (fromIntegral rot)) .|. (xorShifted `shiftL` fromIntegral ((-rot) .&. 31))
--       in (out, PCGen newState inc)
-- :}
--
-- >>> fst $ stepGen $ snd $ stepGen (PCGen 17 29)
-- 3288430965
--
-- >>> :{
-- instance RandomGen PCGen where
--   next g = (fromIntegral y, h)
--     where
--       (y, h) = stepGen g
--   genRange _ = (0, 2 ^ 31)
-- :}
--
-- Importantly, this implementation will not be as efficient as it
-- could be because the random values are converted to 'Integer' and
-- then to desired type.
--
-- Instead we should define (where e.g. @unBuildWord32 :: Word32 ->
-- (Word16, Word16)@ is a function to pull apart a 'Word32' into a
-- pair of 'Word16'):
--
-- >>> newtype PCGen' = PCGen' { unPCGen :: PCGen }
--
-- >>> :{
-- let stepGen' :: PCGen' -> (Word32, PCGen')
--     stepGen' = second PCGen' . stepGen . unPCGen
-- :}
--
-- >>> :set -fno-warn-missing-methods
--
-- >>> :{
-- instance RandomGen PCGen' where
--   generate = first fromIntegral . stepGen'
--   entropy _ = 31
-- :}
--
-- [/Example for RNG Users:/]
--
-- Suppose you want to simulate rolls from a dice (yes I know it's a
-- plural form but it's now common to use it as a singular form):
--
-- >>> :{
-- let randomListM :: (MonadRandom g m, Num a, UniformRange a) => Int -> (a, a) -> g -> m [a]
--     randomListM n range gen = replicateM n $ uniformR range gen
-- :}
--
-- >>> :{
-- let rolls :: [Word32]
--     rolls = runGenState_
--               (PCGen 17 29)
--               (randomListM 10 (1, 6) PureGenI)
-- :}
--
-- >>> rolls
-- [1,6,6,1,3,5,3,1,6,5]
--
-- FIXME: What should we say about generating values from types other
-- than Word8 etc?
--
-----------------------------------------------------------------------------

module System.Random
  (

  -- $intro

  -- * Random number generators

    RandomGen(..)
  , MonadRandom(..)
  , withGenM
  -- ** Standard random number generators
  , StdGen
  , mkStdGen

  -- * Stateful interface for pure generators
  -- ** Based on StateT
  , PureGen
  , splitGen
  , genRandom
  , runGenState
  , runGenState_
  , runGenStateT
  , runGenStateT_
  , runPureGenST
  -- ** Based on PrimMonad
  -- *** PrimGen - boxed thread safe state
  , PrimGen
  , runPrimGenST
  , runPrimGenST_
  , runPrimGenIO
  , runPrimGenIO_
  , splitPrimGen
  , atomicPrimGen
  -- *** MutGen - unboxed mutable state
  , MutGen
  , runMutGenST
  , runMutGenST_
  , runMutGenIO
  , runMutGenIO_
  , splitMutGen
  , applyMutGen

  -- ** The global random number generator

  -- $globalrng

  , getStdRandom
  , getStdGen
  , setStdGen
  , newStdGen

  -- * Random values of various types
  , Uniform(..)
  , UniformRange(..)
  , Random(..)

  -- * Generators for sequences of bytes
  , uniformByteArrayPrim
  , uniformByteStringPrim
  , genByteArray
  , genByteString

  -- * References
  -- $references

  -- * Internals
  , bitmaskWithRejection -- FIXME Export this in a better way, e.g. in System.Random.Impl or something like that
  ) where

import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Bits
import Data.ByteString.Builder.Prim (word64LE)
import Data.ByteString.Builder.Prim.Internal (runF)
import Data.ByteString.Internal (ByteString(PS))
import Data.ByteString.Short.Internal (ShortByteString(SBS), fromShort)
import Data.Int
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Primitive.ByteArray
import Data.Primitive.MutVar
import Data.Primitive.Types as Primitive (Prim, sizeOf)
import Data.Proxy
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Exts (Ptr(..), build)
import GHC.ForeignPtr
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random.SplitMix as SM

#if !MIN_VERSION_primitive(0,7,0)
import Data.Primitive.Types (Addr(..))

mutableByteArrayContentsCompat mba =
  case mutableByteArrayContents mba of
    Addr addr# -> Ptr addr#
#else
mutableByteArrayContentsCompat = mutableByteArrayContents
#endif
mutableByteArrayContentsCompat :: MutableByteArray s -> Ptr Word8
{-# INLINE mutableByteArrayContentsCompat #-}

-- | A source of random bits.
{-# DEPRECATED next "Use 'generate' instead" #-}
{-# DEPRECATED genRange "Use 'entropy' instead" #-}
class RandomGen g where
  {-# MINIMAL (generate,entropy)|(next,genRange) #-}
  -- | Generate random bits.
  --
  -- The resulting 'Word64' is treated as a bitstring of length 'entropy'. All
  -- but the 'entropy' least significant bits must be zero, i.e. for any @g@,
  -- > countLeadingZeros bits >= (64 - entropy) where (bits, _) = generate g
  generate :: g -> (Word64, g)
  generate g = (fromIntegral $ i - lo, g')
    where
      (lo, _) = genRange g
      (i, g') = next g
  {-# INLINE generate #-}

  -- | How many bits of entropy this generator provides per call to 'generate'.
  --
  -- 'entropy' must be greater than zero and cannot exceed 64.
  -- > 0 < entropy (Proxy :: Proxy g) <= 64
  entropy :: Proxy g -> Int
  entropy (_ :: Proxy g) = 64 - countLeadingZeros diff
    where
      (lo, hi) = genRange (undefined :: g)
      diff :: Word64 = fromIntegral $ hi - lo
  {-# INLINE entropy #-}

  next :: g -> (Int, g)
  next = first fromIntegral . generate
  {-# INLINE next #-}

  genRange :: g -> (Int, Int)
  genRange (_ :: g) = (0, 1 `shiftL` (e - 1))
    where
      e = entropy (Proxy :: Proxy g)
  {-# INLINE genRange #-}

  -- | Split this generator into two.
  split :: g -> (g, g)
  split _ = error "RandomGen.split not supported"

-- | Fill a 'Word64' with exactly 'n' random bits.
--
-- 'n' cannot exceed 64.
--
-- > (bits, _) <- generateNM n g
-- > countLeadingZeros bits >= (64 - n)
generateNM :: (Monad m, MonadRandom g m) => Int -> g -> m Word64
generateNM n (gen :: g) = do
  s <- entropyM (Proxy :: Proxy g)
  bits <- generateM gen
  if n <= s
    then return $ bits .&. mask
    else go s bits
  where
    mask = complement zeroBits `shiftR` (64 - n)

    go :: (Monad m, MonadRandom g m) => Int -> Word64 -> m Word64
    go i acc
      | i >= n = return $ acc .&. mask
      | otherwise = do
          s <- entropyM (Proxy :: Proxy g)
          bits <- generateM gen
          let acc' = (acc `shiftL` s) .|. bits
          go (i + s) acc'
{-# INLINE generateNM #-}

genByteArray :: RandomGen g => Int -> g -> (ByteArray, g)
genByteArray n g = runPureGenST g $ uniformByteArrayPrim n

class Monad m => MonadRandom g m where
  data Frozen g :: *

  thawGen :: Frozen g -> m g
  freezeGen :: g -> m (Frozen g)

  generateM :: g -> m Word64
  entropyM :: Proxy g -> m Int

withGenM :: MonadRandom g m => Frozen g -> (g -> m a) -> m (a, Frozen g)
withGenM fg action = do
  g <- thawGen fg
  res <- action g
  fg' <- freezeGen g
  pure (res, fg')


-- | This function will efficiently generate a sequence of random bytes in a platform
-- independent manner. Memory allocated will be pinned, so it is safe to use for FFI
-- calls.
uniformByteArrayPrim :: (MonadRandom g m, PrimMonad m) => Int -> g -> m ByteArray
uniformByteArrayPrim n0 gen = do
  let n = max 0 n0
      (n64, nrem64) = n `quotRem` 8
  ma <- newPinnedByteArray n
  let go i ptr
        | i < n64 = do
          w64 <- generateNM 64 gen
          -- Writing 8 bytes at a time in a Little-endian order gives us platform
          -- portability
          unsafeIOToPrim $ runF word64LE w64 ptr
          go (i + 1) (ptr `plusPtr` 8)
        | otherwise = return ptr
  ptr <- go 0 (mutableByteArrayContentsCompat ma)
  when (nrem64 > 0) $ do
    w64 <- generateNM 64 gen
    -- In order to not mess up the byte order we write generated Word64 into a temporary
    -- pointer and then copy only the missing bytes over to the array. It is tempting to
    -- simply generate as many bytes as we still need using smaller generators
    -- (eg. uniformWord8), but that would result in inconsistent tail when total length is
    -- slightly varied.
    unsafeIOToPrim $
      alloca $ \w64ptr -> do
        runF word64LE w64 w64ptr
        forM_ [0 .. nrem64 - 1] $ \i -> do
          w8 :: Word8 <- peekByteOff w64ptr i
          pokeByteOff ptr i w8
  unsafeFreezeByteArray ma
{-# INLINE uniformByteArrayPrim #-}


pinnedMutableByteArrayToByteString :: MutableByteArray RealWorld -> ByteString
pinnedMutableByteArrayToByteString mba =
  PS (pinnedMutableByteArrayToForeignPtr mba) 0 (sizeofMutableByteArray mba)
{-# INLINE pinnedMutableByteArrayToByteString #-}

pinnedMutableByteArrayToForeignPtr :: MutableByteArray RealWorld -> ForeignPtr a
pinnedMutableByteArrayToForeignPtr mba@(MutableByteArray mba#) =
  case mutableByteArrayContentsCompat mba of
    Ptr addr# -> ForeignPtr addr# (PlainPtr mba#)
{-# INLINE pinnedMutableByteArrayToForeignPtr #-}

-- | Generate a ByteString using a pure generator. For monadic counterpart see
-- `uniformByteStringPrim`.
--
-- @since 1.2
uniformByteStringPrim ::
     (MonadRandom g m, PrimMonad m) => Int -> g -> m ByteString
uniformByteStringPrim n g = do
  ba@(ByteArray ba#) <- uniformByteArrayPrim n g
  if isByteArrayPinned ba
    then unsafeIOToPrim $
         pinnedMutableByteArrayToByteString <$> unsafeThawByteArray ba
    else return $ fromShort (SBS ba#)
{-# INLINE uniformByteStringPrim #-}

-- | Generate a ByteString using a pure generator. For monadic counterpart see
-- `uniformByteStringPrim`.
--
-- @since 1.2
genByteString :: RandomGen g => Int -> g -> (ByteString, g)
genByteString n g = runPureGenST g (uniformByteStringPrim n)
{-# INLINE genByteString #-}

-- | Run an effectful generating action in `ST` monad using a pure generator.
--
-- @since 1.2
runPureGenST :: RandomGen g => g -> (forall s . PureGen g -> StateT g (ST s) a) -> (a, g)
runPureGenST g action = runST $ runGenStateT g $ action PureGenI
{-# INLINE runPureGenST #-}


-- | An opaque data type that carries the type of a pure generator
data PureGen g = PureGenI

instance (MonadState g m, RandomGen g) => MonadRandom (PureGen g) m where
  newtype Frozen (PureGen g) = PureGen g
  thawGen (PureGen g) = PureGenI <$ put g
  freezeGen _ = fmap PureGen get

  generateM _ = state generate
  entropyM _ = pure $ entropy (Proxy :: Proxy g)

-- | Generate a random value in a state monad
--
-- @since 1.2
genRandom :: (RandomGen g, Random a, MonadState g m) => m a
genRandom = randomM PureGenI

-- | Split current generator and update the state with one part, while returning the other.
--
-- @since 1.2
splitGen :: (MonadState g m, RandomGen g) => m g
splitGen = state split

runGenState :: RandomGen g => g -> State g a -> (a, g)
runGenState = flip runState

runGenState_ :: RandomGen g => g -> State g a -> a
runGenState_ g = fst . flip runState g

runGenStateT :: RandomGen g => g -> StateT g m a -> m (a, g)
runGenStateT = flip runStateT

runGenStateT_ :: (RandomGen g, Functor f) => g -> StateT g f a -> f a
runGenStateT_ g = fmap fst . flip runStateT g

-- | This is a wrapper wround pure generator that can be used in an effectful environment.
-- It is safe in presence of concurrency since all operations are performed atomically.
--
-- @since 1.2
newtype PrimGen s g = PrimGenI (MutVar s g)

instance (s ~ PrimState m, PrimMonad m, RandomGen g) =>
         MonadRandom (PrimGen s g) m where
  newtype Frozen (PrimGen s g) = PrimGen g
  thawGen (PrimGen g) = fmap PrimGenI (newMutVar g)
  freezeGen (PrimGenI gVar) = fmap PrimGen (readMutVar gVar)
  generateM = atomicPrimGen generate
  entropyM _ = pure $ entropy (Proxy :: Proxy g)

-- | Apply a pure operation to generator atomically.
atomicPrimGen :: PrimMonad m => (g -> (a, g)) -> PrimGen (PrimState m) g -> m a
atomicPrimGen op (PrimGenI gVar) =
  atomicModifyMutVar' gVar $ \g ->
    case op g of
      (a, g') -> (g', a)
{-# INLINE atomicPrimGen #-}


-- | Split `PrimGen` into atomically updated current generator and a newly created that is
-- returned.
--
-- @since 1.2
splitPrimGen ::
     (RandomGen g, PrimMonad m)
  => PrimGen (PrimState m) g
  -> m (PrimGen (PrimState m) g)
splitPrimGen = atomicPrimGen split >=> thawGen . PrimGen

runPrimGenST :: RandomGen g => g -> (forall s . PrimGen s g -> ST s a) -> (a, g)
runPrimGenST g action = runST $ do
  primGen <- thawGen $ PrimGen g
  res <- action primGen
  PrimGen g' <- freezeGen primGen
  pure (res, g')

-- | Same as `runPrimGenST`, but discard the resulting generator.
runPrimGenST_ :: RandomGen g => g -> (forall s . PrimGen s g -> ST s a) -> a
runPrimGenST_ g action = fst $ runPrimGenST g action

runPrimGenIO :: (RandomGen g, MonadIO m) => g -> (PrimGen RealWorld g -> m a) -> m (a, g)
runPrimGenIO g action = do
  primGen <- liftIO $ thawGen $ PrimGen g
  res <- action primGen
  PrimGen g' <- liftIO $ freezeGen primGen
  pure (res, g')
{-# INLINE runPrimGenIO #-}

-- | Same as `runPrimGenIO`, but discard the resulting generator.
runPrimGenIO_ :: (RandomGen g, MonadIO m) => g -> (PrimGen RealWorld g -> m a) -> m a
runPrimGenIO_ g action = fst <$> runPrimGenIO g action
{-# INLINE runPrimGenIO_ #-}


newtype MutGen s g = MutGenI (MutableByteArray s)

instance (s ~ PrimState m, PrimMonad m, RandomGen g, Prim g) =>
         MonadRandom (MutGen s g) m where
  newtype Frozen (MutGen s g) = MutGen g
  thawGen (MutGen g) = do
    ma <- newByteArray (Primitive.sizeOf g)
    writeByteArray ma 0 g
    pure $ MutGenI ma
  freezeGen (MutGenI ma) = MutGen <$> readByteArray ma 0
  generateM = applyMutGen generate
  entropyM _ = pure $ entropy (Proxy :: Proxy g)

applyMutGen :: (Prim g, PrimMonad m) => (g -> (a, g)) -> MutGen (PrimState m) g -> m a
applyMutGen f (MutGenI ma) = do
  g <- readByteArray ma 0
  case f g of
    (res, g') -> res <$ writeByteArray ma 0 g'

-- | Split `MutGen` into atomically updated current generator and a newly created that is
-- returned.
--
-- @since 1.2
splitMutGen ::
     (Prim g, RandomGen g, PrimMonad m)
  => MutGen (PrimState m) g
  -> m (MutGen (PrimState m) g)
splitMutGen = applyMutGen split >=> thawGen . MutGen

runMutGenST :: (Prim g, RandomGen g) => g -> (forall s . MutGen s g -> ST s a) -> (a, g)
runMutGenST g action = runST $ do
  mutGen <- thawGen $ MutGen g
  res <- action mutGen
  MutGen g' <- freezeGen mutGen
  pure (res, g')

-- | Same as `runMutGenST`, but discard the resulting generator.
runMutGenST_ :: (Prim g, RandomGen g) => g -> (forall s . MutGen s g -> ST s a) -> a
runMutGenST_ g action = fst $ runMutGenST g action

runMutGenIO :: (Prim g, RandomGen g, MonadIO m) => g -> (MutGen RealWorld g -> m a) -> m (a, g)
runMutGenIO g action = do
  mutGen <- liftIO $ thawGen $ MutGen g
  res <- action mutGen
  MutGen g' <- liftIO $ freezeGen mutGen
  pure (res, g')

-- | Same as `runMutGenIO`, but discard the resulting generator.
runMutGenIO_ :: (Prim g, RandomGen g, MonadIO m) => g -> (MutGen RealWorld g -> m a) -> m a
runMutGenIO_ g action = fst <$> runMutGenIO g action

type StdGen = SM.SMGen

instance RandomGen StdGen where
  generate = SM.nextWord64
  entropy _ = 64
  split = SM.splitSMGen

{- |
The function 'mkStdGen' provides an alternative way of producing an initial
generator, by mapping an 'Int' into a generator. Again, distinct arguments
should be likely to produce distinct generators.
-}
mkStdGen :: Int -> StdGen
mkStdGen s = SM.mkSMGen $ fromIntegral s

class Uniform a where
  uniform :: MonadRandom g m => g -> m a

class UniformRange a where
  uniformR :: MonadRandom g m => (a, a) -> g -> m a

-- | Generate a random 'FiniteBits'.
--
-- Requires @finiteBitSize x <= 64@ since the underlying randomness generation
-- is based on 64-bit numbers.
generateFiniteBitsM :: (MonadRandom g m, FiniteBits a, Integral a) => a -> g -> m a
generateFiniteBitsM (_ :: a) = fmap fromIntegral . generateNM n
  where n = finiteBitSize (undefined :: a)
{-# INLINE generateFiniteBitsM #-}

instance Uniform Word8  where uniform = generateFiniteBitsM zeroBits
instance Uniform Word16 where uniform = generateFiniteBitsM zeroBits
instance Uniform Word32 where uniform = generateFiniteBitsM zeroBits
instance Uniform Word64 where uniform = generateFiniteBitsM zeroBits

instance Uniform Int8  where uniform = generateFiniteBitsM zeroBits
instance Uniform Int16 where uniform = generateFiniteBitsM zeroBits
instance Uniform Int32 where uniform = generateFiniteBitsM zeroBits
instance Uniform Int64 where uniform = generateFiniteBitsM zeroBits

#if WORD_SIZE_IN_BITS == 32
fromFixedW :: Word32 -> Word
fromFixedI :: Int32 -> Int
toFixedW :: Word -> Word32
toFixedI :: Int -> Int32
#elif WORD_SIZE_IN_BITS == 64
fromFixedW :: Word64 -> Word
fromFixedI :: Int64 -> Int
toFixedW :: Word -> Word64
toFixedI :: Int -> Int64
#else
#error "unknown word size"
#endif
fromFixedW = fromIntegral
toFixedW = fromIntegral
fromFixedI = fromIntegral
toFixedI = fromIntegral

instance Uniform Word where uniform = fmap fromFixedW . uniform
instance Uniform Int where uniform = fmap fromFixedI . uniform

instance Uniform Char where uniform = fmap toEnum . uniform
instance Uniform Bool where uniform = fmap (flip testBit 1) . generateNM 1

instance Uniform CChar where uniform = generateFiniteBitsM zeroBits
instance Uniform CIntMax where uniform = generateFiniteBitsM zeroBits
instance Uniform CIntPtr where uniform = generateFiniteBitsM zeroBits
instance Uniform CLLong where uniform = generateFiniteBitsM zeroBits
instance Uniform CInt where uniform = generateFiniteBitsM zeroBits
instance Uniform CULLong where uniform = generateFiniteBitsM zeroBits
instance Uniform CPtrdiff where uniform = generateFiniteBitsM zeroBits
instance Uniform CSChar where uniform = generateFiniteBitsM zeroBits
instance Uniform CShort where uniform = generateFiniteBitsM zeroBits
instance Uniform CSigAtomic where uniform = generateFiniteBitsM zeroBits
instance Uniform CSize where uniform = generateFiniteBitsM zeroBits
instance Uniform CUChar where uniform = generateFiniteBitsM zeroBits
instance Uniform CUIntPtr where uniform = generateFiniteBitsM zeroBits
instance Uniform CUInt where uniform = generateFiniteBitsM zeroBits
instance Uniform CUIntMax where uniform = generateFiniteBitsM zeroBits
instance Uniform CULong where uniform = generateFiniteBitsM zeroBits
instance Uniform CLong where uniform = generateFiniteBitsM zeroBits
instance Uniform CUShort where uniform = generateFiniteBitsM zeroBits
instance Uniform CWchar where uniform = generateFiniteBitsM zeroBits

instance UniformRange Word8  where uniformR = bitmaskWithRejectionM
instance UniformRange Word16 where uniformR = bitmaskWithRejectionM
instance UniformRange Word32 where uniformR = bitmaskWithRejectionM
instance UniformRange Word64 where uniformR = bitmaskWithRejectionM

instance UniformRange Word where
  uniformR (lo, hi) = fmap fromFixedW . uniformR (toFixedW lo, toFixedW hi)
instance UniformRange Int where
  uniformR (lo, hi) = fmap fromFixedI . uniformR (toFixedI lo, toFixedI hi)

uniformREnum :: (Functor m, MonadRandom g m, Enum a) => (a, a) -> g -> m a
uniformREnum (lo, hi) = fmap toEnum . bitmaskWithRejectionM (fromEnum lo, fromEnum hi)

instance UniformRange Bool where uniformR = uniformREnum
instance UniformRange Char where uniformR = uniformREnum

instance UniformRange Int8  where uniformR = bitmaskWithRejectionM
instance UniformRange Int16 where uniformR = bitmaskWithRejectionM
instance UniformRange Int32 where uniformR = bitmaskWithRejectionM
instance UniformRange Int64 where uniformR = bitmaskWithRejectionM

instance UniformRange CChar where uniformR = bitmaskWithRejectionM
instance UniformRange CIntMax where uniformR = bitmaskWithRejectionM
instance UniformRange CIntPtr where uniformR = bitmaskWithRejectionM
instance UniformRange CLLong where uniformR = bitmaskWithRejectionM
instance UniformRange CLong where uniformR = bitmaskWithRejectionM
instance UniformRange CInt where uniformR = bitmaskWithRejectionM
instance UniformRange CPtrdiff where uniformR = bitmaskWithRejectionM
instance UniformRange CSChar where uniformR = bitmaskWithRejectionM
instance UniformRange CShort where uniformR = bitmaskWithRejectionM
instance UniformRange CSigAtomic where uniformR = bitmaskWithRejectionM
instance UniformRange CSize where uniformR = bitmaskWithRejectionM
instance UniformRange CUChar where uniformR = bitmaskWithRejectionM
instance UniformRange CUInt where uniformR = bitmaskWithRejectionM
instance UniformRange CUIntPtr where uniformR = bitmaskWithRejectionM
instance UniformRange CULLong where uniformR = bitmaskWithRejectionM
instance UniformRange CUIntMax where uniformR = bitmaskWithRejectionM
instance UniformRange CULong where uniformR = bitmaskWithRejectionM
instance UniformRange CUShort where uniformR = bitmaskWithRejectionM
instance UniformRange CWchar where uniformR = bitmaskWithRejectionM

{- |
With a source of random number supply in hand, the 'Random' class allows the
programmer to extract random values of a variety of types.
-}
{-# DEPRECATED randomR "In favor of `uniformR`" #-}
{-# DEPRECATED randomRIO "In favor of `uniformR`" #-}
{-# DEPRECATED randomIO "In favor of `uniformR`" #-}
class Random a where

  -- | Takes a range /(lo,hi)/ and a random number generator
  -- /g/, and returns a random value uniformly distributed in the closed
  -- interval /[lo,hi]/, together with a new generator. It is unspecified
  -- what happens if /lo>hi/. For continuous types there is no requirement
  -- that the values /lo/ and /hi/ are ever produced, but they may be,
  -- depending on the implementation and the interval.
  {-# INLINE randomR #-}
  randomR :: RandomGen g => (a, a) -> g -> (a, g)
  default randomR :: (RandomGen g, UniformRange a) => (a, a) -> g -> (a, g)
  randomR r g = runGenState g (uniformR r PureGenI)

  -- | The same as 'randomR', but using a default range determined by the type:
  --
  -- * For bounded types (instances of 'Bounded', such as 'Char'),
  --   the range is normally the whole type.
  --
  -- * For fractional types, the range is normally the semi-closed interval
  -- @[0,1)@.
  --
  -- * For 'Integer', the range is (arbitrarily) the range of 'Int'.
  {-# INLINE random #-}
  random  :: RandomGen g => g -> (a, g)
  random g = runGenState g genRandom

  --{-# INLINE randomM #-}
  randomM :: MonadRandom g m => g -> m a
  -- default randomM :: (MonadRandom g m, Uniform a) => g -> m a
  -- randomM = uniform

  -- | Plural variant of 'randomR', producing an infinite list of
  -- random values instead of returning a new generator.
  {-# INLINE randomRs #-}
  randomRs :: RandomGen g => (a,a) -> g -> [a]
  randomRs ival g = build (\cons _nil -> buildRandoms cons (randomR ival) g)

  -- | Plural variant of 'random', producing an infinite list of
  -- random values instead of returning a new generator.
  {-# INLINE randoms #-}
  randoms  :: RandomGen g => g -> [a]
  randoms  g      = build (\cons _nil -> buildRandoms cons random g)

  -- | A variant of 'randomR' that uses the global random number generator
  -- (see "System.Random#globalrng").
  randomRIO :: (a,a) -> IO a
  randomRIO range  = getStdRandom (randomR range)

  -- | A variant of 'random' that uses the global random number generator
  -- (see "System.Random#globalrng").
  randomIO  :: IO a
  randomIO   = getStdRandom random

-- | Produce an infinite list-equivalent of random values.
{-# INLINE buildRandoms #-}
buildRandoms :: RandomGen g
             => (a -> as -> as)  -- ^ E.g. '(:)' but subject to fusion
             -> (g -> (a,g))     -- ^ E.g. 'random'
             -> g                -- ^ A 'RandomGen' instance
             -> as
buildRandoms cons rand = go
  where
    -- The seq fixes part of #4218 and also makes fused Core simpler.
    go g = x `seq` (x `cons` go g') where (x,g') = rand g

instance UniformRange Integer where
  -- TODO: This is super broken, do this properly!
  uniformR (lo, hi) = fmap (toInteger :: Int -> Integer) . uniformR (fromIntegral lo, fromIntegral hi)

instance Random Integer where
  random g = randomR (toInteger (minBound::Int), toInteger (maxBound::Int)) g
  randomM g = uniformR (toInteger (minBound::Int), toInteger (maxBound::Int)) g

instance Random Int8 where randomM = uniform 
instance Random Int16 where randomM = uniform
instance Random Int32 where randomM = uniform
instance Random Int64 where randomM = uniform

instance Random Word8 where randomM = uniform
instance Random Word16 where randomM = uniform
instance Random Word32 where randomM = uniform
instance Random Word64 where randomM = uniform

instance Random Int where randomM = uniform
instance Random Word where randomM = uniform

instance Random CChar where randomM = uniform
instance Random CSChar where randomM = uniform
instance Random CUChar where randomM = uniform
instance Random CShort where randomM = uniform
instance Random CUShort where randomM = uniform
instance Random CInt where randomM = uniform
instance Random CUInt where randomM = uniform
instance Random CLong where randomM = uniform
instance Random CULong where randomM = uniform
instance Random CPtrdiff where randomM = uniform
instance Random CSize where randomM = uniform
instance Random CWchar where randomM = uniform
instance Random CSigAtomic where randomM = uniform
instance Random CLLong where randomM = uniform
instance Random CULLong where randomM = uniform
instance Random CIntPtr where randomM = uniform
instance Random CUIntPtr where randomM = uniform
instance Random CIntMax where randomM = uniform
instance Random CUIntMax where randomM = uniform

instance Random Char where randomM = uniform
instance Random Bool where randomM = uniform

{-# INLINE randomRFloating #-}
randomRFloating :: (Fractional a, Num a, Ord a, Random a, RandomGen g) => (a, a) -> g -> (a, g)
randomRFloating (l,h) g
    | l>h       = randomRFloating (h,l) g
    | otherwise = let (coef,g') = random g in
                    (2.0 * (0.5*l + coef * (0.5*h - 0.5*l)), g')  -- avoid overflow

instance Random Double where
  randomR = randomRFloating
  random = randomDouble
  -- TODO
  -- randomM = uniformR (0, 1)

randomDouble :: RandomGen b => b -> (Double, b)
randomDouble rng =
    case random rng of
      (x,rng') ->
          -- We use 53 bits of randomness corresponding to the 53 bit significand:
          ((fromIntegral (mask53 .&. (x::Int64)) :: Double)
           /  fromIntegral twoto53, rng')
   where
    twoto53 = (2::Int64) ^ (53::Int64)
    mask53 = twoto53 - 1


instance Random Float where
  randomR = randomRFloating
  random = randomFloat
  -- TODO
  -- randomM = uniformR (0, 1)

randomFloat :: RandomGen b => b -> (Float, b)
randomFloat rng =
    -- TODO: Faster to just use 'next' IF it generates enough bits of randomness.
    case random rng of
      (x,rng') ->
          -- We use 24 bits of randomness corresponding to the 24 bit significand:
          ((fromIntegral (mask24 .&. (x::Int32)) :: Float)
           /  fromIntegral twoto24, rng')
          -- Note, encodeFloat is another option, but I'm not seeing slightly
          --  worse performance with the following [2011.06.25]:
--         (encodeFloat rand (-24), rng')
   where
     mask24 = twoto24 - 1
     twoto24 = (2::Int32) ^ (24::Int32)

-- CFloat/CDouble are basically the same as a Float/Double:
-- instance Random CFloat where
--   randomR = randomRFloating
  -- random rng = case random rng of
  --              (x,rng') -> (realToFrac (x::Float), rng')

-- instance Random CDouble where
--   randomR = randomRFloating
--   -- A MYSTERY:
--   -- Presently, this is showing better performance than the Double instance:
--   -- (And yet, if the Double instance uses randomFrac then its performance is much worse!)
--   random  = randomFrac
--   -- random rng = case random rng of
--   --                  (x,rng') -> (realToFrac (x::Double), rng')

bitmaskWithRejection ::
     (RandomGen g, FiniteBits a, Num a, Ord a, Random a)
  => (a, a)
  -> g
  -> (a, g)
bitmaskWithRejection (bottom, top)
  | bottom > top = bitmaskWithRejection (top, bottom)
  | bottom == top = (,) top
  | otherwise = first (bottom +) . go
  where
    range = top - bottom
    mask = complement zeroBits `shiftR` countLeadingZeros (range .|. 1)
    go g =
      let (x, g') = random g
          x' = x .&. mask
       in if x' > range
            then go g'
            else (x', g')
{-# INLINE bitmaskWithRejection #-}

bitmaskWithRejectionM :: (Ord a, Integral a, FiniteBits a, Num a, MonadRandom g m) => (a, a) -> g -> m a
bitmaskWithRejectionM (lo, hi) gen
  | lo > hi = bitmaskWithRejectionM (hi, lo) gen
  | lo == hi = pure lo
  | otherwise = fmap (+ lo) go
  where
    range = hi - lo
    mask = complement zeroBits `shiftR` countLeadingZeros (range .|. 1)
    go = do
      x <- generateFiniteBitsM zeroBits gen
      let x' = x .&. mask
      if x' > range
        then go
        else pure x'
{-# INLINE bitmaskWithRejectionM #-}

-- The global random number generator

{- $globalrng #globalrng#

There is a single, implicit, global random number generator of type
'StdGen', held in some global variable maintained by the 'IO' monad. It is
initialised automatically in some system-dependent fashion, for example, by
using the time of day, or Linux's kernel random number generator. To get
deterministic behaviour, use 'setStdGen'.
-}

-- |Sets the global random number generator.
setStdGen :: StdGen -> IO ()
setStdGen sgen = writeIORef theStdGen sgen

-- |Gets the global random number generator.
getStdGen :: IO StdGen
getStdGen  = readIORef theStdGen

theStdGen :: IORef StdGen
theStdGen  = unsafePerformIO $ SM.initSMGen >>= newIORef
{-# NOINLINE theStdGen #-}

-- |Applies 'split' to the current global random generator,
-- updates it with one of the results, and returns the other.
newStdGen :: IO StdGen
newStdGen = atomicModifyIORef' theStdGen split

{- |Uses the supplied function to get a value from the current global
random generator, and updates the global generator with the new generator
returned by the function. For example, @rollDice@ gets a random integer
between 1 and 6:

>  rollDice :: IO Int
>  rollDice = getStdRandom (randomR (1,6))

-}

getStdRandom :: (StdGen -> (a,StdGen)) -> IO a
getStdRandom f = atomicModifyIORef' theStdGen (swap . f)
  where swap (v,g) = (g,v)

{- $references

1. Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014. Fast splittable
pseudorandom number generators. In Proceedings of the 2014 ACM International
Conference on Object Oriented Programming Systems Languages & Applications
(OOPSLA '14). ACM, New York, NY, USA, 453-472. DOI:
https://doi.org/10.1145/2660193.2660195

-}
