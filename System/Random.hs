{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedFFITypes #-}

#include "MachDeps.h"

-- |
-- Module      :  System.Random
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
--
-- This library deals with the common task of pseudo-random number generation.
module System.Random
  (
  -- * Introduction
  -- $introduction

  -- * Usage
  -- ** How to generate pseudo-random values in monadic code
  -- $usagemonadic

  -- ** How to generate pseudo-random values in pure code
  -- $usagepure

  -- * Pure and monadic pseudo-random number generator interfaces
  -- $interfaces
    RandomGen(..)
  , MonadRandom(..)
  , Frozen(..)
  , runGenM
  , runGenM_
  , RandomGenM(..)
  , splitRandomGenM

  -- ** Standard pseudo-random number generator
  , StdGen
  , mkStdGen

  -- ** Global standard pseudo-random number generator
  -- $globalstdgen
  , getStdRandom
  , getStdGen
  , setStdGen
  , newStdGen

  -- * Monadic adapters for pure pseudo-random number generators
  -- $monadicadapters

  -- ** Pure adapter
  , PureGen
  , splitGen
  , genRandom
  , runGenState
  , runGenState_
  , runGenStateT
  , runGenStateT_
  , runPureGenST
  -- ** Mutable adapter with atomic operations
  , AtomicGen
  , applyAtomicGen
  -- ** Mutable adapter in 'IO'
  , IOGen
  , applyIOGen
  -- ** Mutable adapter in 'ST'
  , STGen
  , applySTGen
  , runSTGen
  , runSTGen_

  -- * Pseudo-random values of various types
  -- $uniform
  , Uniform(..)
  , uniform
  , uniformR
  , uniformListM
  , UniformRange(..)
  , Random(..)

  -- * Generators for sequences of pseudo-random bytes
  , genShortByteStringIO
  , genShortByteStringST
  , uniformByteString
  , genByteString

  -- * Compatibility and reproducibility
  -- ** Backwards compatibility and deprecations
  -- $deprecations

  -- ** Reproducibility
  -- $reproducibility

  -- * Notes for pseudo-random number generator implementors
  -- ** How to implement 'RandomGen'
  -- $implementrandomgen

  -- ** How to implement 'MonadRandom'
  -- $implementmonadrandom

  -- * References
  -- $references
  ) where

import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad.State.Strict
import Data.Bits
import Data.ByteString.Internal (ByteString(PS))
import Data.ByteString.Short.Internal (ShortByteString(SBS), fromShort)
import Data.Int
import Data.IORef
import Data.STRef
import Data.Word
import Foreign.C.Types
import GHC.Exts
import GHC.ForeignPtr
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random.SplitMix as SM
import qualified System.Random.SplitMix32 as SM32
import GHC.Word

import System.Random.Internal
import System.Random.Monad

-- $introduction
--
-- This library provides type classes and instances for the following concepts:
--
-- [Pure pseudo-random number generators] 'RandomGen' is an interface to pure
--     pseudo-random number generators.
--
--     'StdGen', the standard pseudo-random number generator provided in this
--     library, is an instance of 'RandomGen'. It uses the SplitMix
--     implementation provided by the
--     <https://hackage.haskell.org/package/splitmix splitmix> package.
--     Programmers may, of course, supply their own instances of 'RandomGen'.
--
-- [Monadic pseudo-random number generators] 'MonadRandom' is an interface to
--     monadic pseudo-random number generators.
--
-- [Monadic adapters] 'PureGen', 'AtomicGen', 'IOGen' and 'STGen' turn a
--     'RandomGen' instance into a 'MonadRandom' instance.
--
-- [Drawing from a range] 'UniformRange' is used to generate a value of a
--     datatype uniformly within an inclusive range.
--
--     This library provides instances of 'UniformRange' for many common
--     numeric datatypes.
--
-- [Drawing from the entire domain of a type] 'Uniform' is used to generate a
--     value of a datatype uniformly over all possible values of that datatype.
--
--     This library provides instances of 'Uniform' for many common bounded
--     numeric datatypes.
--
-- $usagemonadic
--
-- In monadic code, use the relevant 'Uniform' and 'UniformRange' instances to
-- generate pseudo-random values via 'uniformM' and 'uniformRM', respectively.
--
-- As an example, @rolls@ generates @n@ pseudo-random values of @Word8@ in the
-- range @[1, 6]@.
--
-- > rolls :: MonadRandom g s m => Int -> g s -> m [Word8]
-- > rolls n = replicateM n . uniformR (1, 6)
--
-- Given a /monadic/ pseudo-random number generator, you can run this
-- probabilistic computation as follows:
--
-- >>> monadicGen <- MWC.create
-- >>> rolls 10 monadicGen :: IO [Word8]
-- [2,3,6,6,4,4,3,1,5,4]
--
-- Given a /pure/ pseudo-random number generator, you can run it in an 'IO' or
-- 'ST' context by first applying a monadic adapter like 'AtomicGen', 'IOGen'
-- or 'STGen' and then running it with 'runGenM'.
--
-- >>> let pureGen = mkStdGen 42
-- >>> runGenM_ (IOGen pureGen) (rolls 10) :: IO [Word8]
-- [1,1,3,2,4,5,3,4,6,2]
--
-- $usagepure
--
-- In pure code, use 'runGenState' and its variants to extract the pure
-- pseudo-random value from a monadic computation based on a pure pseudo-random
-- number generator.
--
-- >>> let pureGen = mkStdGen 42
-- >>> runGenState_ pureGen (rolls 10) :: [Word8]
-- [1,1,3,2,4,5,3,4,6,2]

-------------------------------------------------------------------------------
-- Pseudo-random number generator interfaces
-------------------------------------------------------------------------------

-- $interfaces
--
-- Pseudo-random number generators come in two flavours: /pure/ and /monadic/.
--
-- ['RandomGen': pure pseudo-random number generators] These generators produce
--     a new pseudo-random value together with a new instance of the
--     pseudo-random number generator.
--
--     Pure pseudo-random number generators should implement 'split' if they
--     are /splittable/, that is, if there is an efficient method to turn one
--     instance of the generator into two such that the pseudo-random numbers
--     produced by the two resulting generators are not correlated. See [1] for
--     some background on splittable pseudo-random generators.
--
-- ['MonadRandom': monadic pseudo-random number generators] These generators
--     mutate their own state as they produce pseudo-random values. They
--     generally live in 'ST' or 'IO' or some transformer that implements
--     @PrimMonad@.
--

-- | 'RandomGen' is an interface to pure pseudo-random number generators.
--
-- 'StdGen' is the standard 'RandomGen' instance provided by this library.
{-# DEPRECATED next "No longer used" #-}
{-# DEPRECATED genRange "No longer used" #-}
class RandomGen g where
  {-# MINIMAL split,(genWord32|genWord64|(next,genRange)) #-}
  -- | Returns an 'Int' that is uniformly distributed over the range returned by
  -- 'genRange' (including both end points), and a new generator. Using 'next'
  -- is inefficient as all operations go via 'Integer'. See
  -- [here](https://alexey.kuleshevi.ch/blog/2019/12/21/random-benchmarks) for
  -- more details. It is thus deprecated.
  next :: g -> (Int, g)
  next g = runGenState g (uniformRM (genRange g))

  -- | Returns a 'Word8' that is uniformly distributed over the entire 'Word8'
  -- range.
  --
  -- @since 1.2
  genWord8 :: g -> (Word8, g)
  genWord8 = first fromIntegral . genWord32

  -- | Returns a 'Word16' that is uniformly distributed over the entire 'Word16'
  -- range.
  --
  -- @since 1.2
  genWord16 :: g -> (Word16, g)
  genWord16 = first fromIntegral . genWord32

  -- | Returns a 'Word32' that is uniformly distributed over the entire 'Word32'
  -- range.
  --
  -- @since 1.2
  genWord32 :: g -> (Word32, g)
  genWord32 = randomIvalIntegral (minBound, maxBound)
  -- Once `next` is removed, this implementation should be used instead:
  -- first fromIntegral . genWord64

  -- | Returns a 'Word64' that is uniformly distributed over the entire 'Word64'
  -- range.
  --
  -- @since 1.2
  genWord64 :: g -> (Word64, g)
  genWord64 g =
    case genWord32 g of
      (l32, g') ->
        case genWord32 g' of
          (h32, g'') ->
            ((fromIntegral h32 `unsafeShiftL` 32) .|. fromIntegral l32, g'')

  -- | @genWord32R upperBound g@ returns a 'Word32' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2
  genWord32R :: Word32 -> g -> (Word32, g)
  genWord32R m g = runGenState g (unbiasedWordMult32 m)

  -- | @genWord64R upperBound g@ returns a 'Word64' that is uniformly
  -- distributed over the range @[0, upperBound]@.
  --
  -- @since 1.2
  genWord64R :: Word64 -> g -> (Word64, g)
  genWord64R m g = runGenState g (unsignedBitmaskWithRejectionM uniformWord64 m)

  -- | @genShortByteString n g@ returns a 'ShortByteString' of length @n@
  -- filled with pseudo-random bytes.
  --
  -- @since 1.2
  genShortByteString :: Int -> g -> (ShortByteString, g)
  genShortByteString n g =
    unsafePerformIO $ runGenStateT g (genShortByteStringIO n . uniformWord64)
  {-# INLINE genShortByteString #-}

  -- | Yields the range of values returned by 'next'.
  --
  -- It is required that:
  --
  -- *   If @(a, b) = 'genRange' g@, then @a < b@.
  -- *   'genRange' must not examine its argument so the value it returns is
  --     determined only by the instance of 'RandomGen'.
  --
  -- The default definition spans the full range of 'Int'.
  genRange :: g -> (Int, Int)
  genRange _ = (minBound, maxBound)

  -- | Returns two distinct pseudo-random number generators.
  --
  -- Implementations should take care to ensure that the resulting generators
  -- are not correlated. Some pseudo-random number generators are not
  -- splittable. In that case, the 'split' implementation should fail with a
  -- descriptive 'error' message.
  split :: g -> (g, g)

-------------------------------------------------------------------------------
-- Monadic adapters
-------------------------------------------------------------------------------

-- $monadicadapters
--
-- Pure pseudo-random number generators can be used in monadic code via the
-- adapters 'PureGen', 'AtomicGen', 'IOGen' and 'STGen'.
--
-- *   'PureGen' can be used in any state monad. With strict 'StateT' there is
--     no performance overhead compared to using the 'RandomGen' instance
--     directly. 'PureGen' is /not/ safe to use in the presence of exceptions
--     and concurrency.
--
-- *   'AtomicGen' is safe in the presence of exceptions and concurrency since
--     it performs all actions atomically.
--
-- *   'IOGen' is a wrapper around an 'IORef' that holds a pure generator.
--     'IOGen' is safe in the presence of exceptions, but not concurrency.
--
-- *   'STGen' is a wrapper around an 'STRef' that holds a pure generator.
--     'STGen' is safe in the presence of exceptions, but not concurrency.

-- | Interface to operations on 'RandomGen' wrappers like 'IOGen' and 'PureGen'.
--
-- @since 1.2
class (RandomGen r, MonadRandom (g r) s m) => RandomGenM g r s m where
  applyRandomGenM :: (r -> (a, r)) -> g r s -> m a

-- | Splits a pseudo-random number generator into two. Overwrites the mutable
-- wrapper with one of the resulting generators and returns the other.
--
-- @since 1.2
splitRandomGenM :: RandomGenM g r s m => g r s -> m r
splitRandomGenM = applyRandomGenM split

instance (RandomGen r, MonadIO m) => RandomGenM IOGen r RealWorld m where
  applyRandomGenM = applyIOGen

instance (RandomGen r, MonadIO m) => RandomGenM AtomicGen r RealWorld m where
  applyRandomGenM = applyAtomicGen

instance (RandomGen r, MonadState r m) => RandomGenM PureGen r r m where
  applyRandomGenM f _ = state f

instance RandomGen r => RandomGenM STGen r s (ST s) where
  applyRandomGenM = applySTGen

-- | Runs a mutable pseudo-random number generator from its 'Frozen' state.
--
-- >>> import Data.Int (Int8)
-- >>> runGenM (IOGen (mkStdGen 217)) (`uniformListM` 5) :: IO ([Int8], Frozen (IOGen StdGen))
-- ([-74,37,-50,-2,3],IOGen {unIOGen = StdGen {unStdGen = SMGen 4273268533320920145 15251669095119325999}})
--
-- @since 1.2
runGenM :: MonadRandom g s m => Frozen g -> (g s -> m a) -> m (a, Frozen g)
runGenM fg action = do
  g <- thawGen fg
  res <- action g
  fg' <- freezeGen g
  pure (res, fg')

-- | Same as 'runGenM', but only returns the generated value.
--
-- @since 1.2
runGenM_ :: MonadRandom g s m => Frozen g -> (g s -> m a) -> m a
runGenM_ fg action = fst <$> runGenM fg action

-- | Generates a list of pseudo-random values.
--
-- @since 1.2
uniformListM :: (MonadRandom g s m, Uniform a) => g s -> Int -> m [a]
uniformListM gen n = replicateM n (uniformM gen)

-- | Same as 'genShortByteStringIO', but runs in 'ST'.
--
-- @since 1.2
genShortByteStringST :: Int -> ST s Word64 -> ST s ShortByteString
genShortByteStringST n action =
  unsafeIOToST (genShortByteStringIO n (unsafeSTToIO action))

pinnedByteArrayToByteString :: ByteArray# -> ByteString
pinnedByteArrayToByteString ba# =
  PS (pinnedByteArrayToForeignPtr ba#) 0 (I# (sizeofByteArray# ba#))
{-# INLINE pinnedByteArrayToByteString #-}

pinnedByteArrayToForeignPtr :: ByteArray# -> ForeignPtr a
pinnedByteArrayToForeignPtr ba# =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE pinnedByteArrayToForeignPtr #-}

-- | Generates a pseudo-random 'ByteString' of the specified size.
--
-- @since 1.2
uniformByteString :: MonadRandom g s m => Int -> g s -> m ByteString
uniformByteString n g = do
  ba@(SBS ba#) <- uniformShortByteString n g
  pure $
    if isTrue# (isByteArrayPinned# ba#)
      then pinnedByteArrayToByteString ba#
      else fromShort ba
{-# INLINE uniformByteString #-}

-- | Generates a 'ByteString' of the specified size using a pure pseudo-random
-- number generator. See 'uniformByteString' for the monadic version.
--
-- @since 1.2
genByteString :: RandomGen g => Int -> g -> (ByteString, g)
genByteString n g = runPureGenST g (uniformByteString n)
{-# INLINE genByteString #-}

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator.
--
-- @since 1.2
runPureGenST :: RandomGen g => g -> (forall s . PureGen g g -> StateT g (ST s) a) -> (a, g)
runPureGenST g action = runST $ runGenStateT g $ action
{-# INLINE runPureGenST #-}


-- | Opaque data type that carries the type of a pure pseudo-random number
-- generator.
--
-- @since 1.2
data PureGen g s = PureGenI

instance (RandomGen g, MonadState g m) => MonadRandom (PureGen g) g m where
  newtype Frozen (PureGen g) = PureGen g
  thawGen (PureGen g) = PureGenI <$ put g
  freezeGen _ = fmap PureGen get
  uniformWord32R r _ = state (genWord32R r)
  uniformWord64R r _ = state (genWord64R r)
  uniformWord8 _ = state genWord8
  uniformWord16 _ = state genWord16
  uniformWord32 _ = state genWord32
  uniformWord64 _ = state genWord64
  uniformShortByteString n _ = state (genShortByteString n)

-- | Pure version of `uniformM` that works with instances of `RandomGen`
--
-- @since 1.2
uniform :: (RandomGen g, Uniform a) => g -> (a, g)
uniform g = runGenState g uniformM


-- | Pure version of `uniformRM` that works with instances of `RandomGen`
--
-- @since 1.2
uniformR :: (RandomGen g, UniformRange a) => g -> (a, a) -> (a, g)
uniformR g r = runGenState g (uniformRM r)

-- | Generates a pseudo-random value in a state monad.
--
-- @since 1.2
genRandom :: (RandomGen g, Random a, MonadState g m) => PureGen g g -> m a
genRandom _ = state random

-- | Splits a pseudo-random number generator into two. Updates the state with
-- one of the resulting generators and returns the other.
--
-- @since 1.2
splitGen :: (MonadState g m, RandomGen g) => m g
splitGen = state split

-- | Runs a monadic generating action in the `State` monad using a pure
-- pseudo-random number generator.
--
-- @since 1.2
runGenState :: RandomGen g => g -> (PureGen g g -> State g a) -> (a, g)
runGenState g f = runState (f PureGenI) g

-- | Runs a monadic generating action in the `State` monad using a pure
-- pseudo-random number generator. Returns only the resulting pseudo-random
-- value.
--
-- @since 1.2
runGenState_ :: RandomGen g => g -> (PureGen g g -> State g a) -> a
runGenState_ g = fst . runGenState g

-- | Runs a monadic generating action in the `StateT` monad using a pure
-- pseudo-random number generator.
--
-- @since 1.2
runGenStateT :: RandomGen g => g -> (PureGen g g -> StateT g m a) -> m (a, g)
runGenStateT g f = runStateT (f PureGenI) g

-- | Runs a monadic generating action in the `StateT` monad using a pure
-- pseudo-random number generator. Returns only the resulting pseudo-random
-- value.
--
-- @since 1.2
runGenStateT_ :: (RandomGen g, Functor f) => g -> (PureGen g g -> StateT g f a) -> f a
runGenStateT_ g = fmap fst . runGenStateT g

-- | This is a wrapper around pure generator that can be used in a monadic
-- environment. It is safe in presence of exceptions and concurrency since all
-- operations are performed atomically.
--
-- @since 1.2
newtype AtomicGen g s = AtomicGenI (IORef g)

instance (RandomGen g, MonadIO m) => MonadRandom (AtomicGen g) RealWorld m where
  newtype Frozen (AtomicGen g) = AtomicGen { unAtomicGen :: g }
    deriving (Eq, Show, Read)
  thawGen (AtomicGen g) = fmap AtomicGenI (liftIO $ newIORef g)
  freezeGen (AtomicGenI gVar) = fmap AtomicGen (liftIO $ readIORef gVar)
  uniformWord32R r = applyAtomicGen (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = applyAtomicGen (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applyAtomicGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applyAtomicGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applyAtomicGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applyAtomicGen genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString n = applyAtomicGen (genShortByteString n)

-- | Atomically applies a pure operation to the wrapped pseudo-random number
-- generator.
--
-- @since 1.2
applyAtomicGen :: MonadIO m => (g -> (a, g)) -> AtomicGen g RealWorld -> m a
applyAtomicGen op (AtomicGenI gVar) =
  liftIO $ atomicModifyIORef' gVar $ \g ->
    case op g of
      (a, g') -> (g', a)
{-# INLINE applyAtomicGen #-}

-- | This is a wrapper around an @IORef@ that holds a pure generator. Because of
-- extra pointer indirection it will be slightly slower than if `PureGen` is
-- being used, but faster than `AtomicGen` wrapper, since atomic modification is
-- not being used with `IOGen`. Which also means that it is not safe in a
-- concurrent setting.
--
-- Both `IOGen` and `AtomicGen` are necessary when generation of pseudo-random
-- values happens in `IO` and especially when dealing with exception handling
-- and resource allocation, which is where `StateT` should never be used. For
-- example writing a pseudo-random number of bytes into a temporary file:
--
-- >>> import UnliftIO.Temporary (withSystemTempFile)
-- >>> import Data.ByteString (hPutStr)
-- >>> let ioGen g = withSystemTempFile "foo.bin" $ \_ h -> uniformRM (0, 100) g >>= flip uniformByteString g >>= hPutStr h
--
-- and then run it:
--
-- >>> runGenM_ (IOGen (mkStdGen 1729)) ioGen
--
-- @since 1.2
newtype IOGen g s = IOGenI (IORef g)

instance (RandomGen g, MonadIO m) => MonadRandom (IOGen g) RealWorld m where
  newtype Frozen (IOGen g) = IOGen { unIOGen :: g }
    deriving (Eq, Show, Read)
  thawGen (IOGen g) = fmap IOGenI (liftIO $ newIORef g)
  freezeGen (IOGenI gVar) = fmap IOGen (liftIO $ readIORef gVar)
  uniformWord32R r = applyIOGen (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = applyIOGen (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applyIOGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applyIOGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applyIOGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applyIOGen genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString n = applyIOGen (genShortByteString n)

-- | Applies a pure operation to the wrapped pseudo-random number generator.
--
-- @since 1.2
applyIOGen :: MonadIO m => (g -> (a, g)) -> IOGen g RealWorld -> m a
applyIOGen f (IOGenI ref) = liftIO $ do
  g <- readIORef ref
  case f g of
    (!a, !g') -> a <$ writeIORef ref g'
{-# INLINE applyIOGen #-}


-- | This is a wrapper wround an @STRef@ that holds a pure generator. Because of
-- extra pointer indirection it will be slightly slower than if `PureGen` is
-- being used.
--
-- @since 1.2
newtype STGen g s = STGenI (STRef s g)

instance RandomGen g => MonadRandom (STGen g) s (ST s) where
  newtype Frozen (STGen g) = STGen { unSTGen :: g }
    deriving (Eq, Show, Read)
  thawGen (STGen g) = fmap STGenI (newSTRef g)
  freezeGen (STGenI gVar) = fmap STGen (readSTRef gVar)
  uniformWord32R r = applySTGen (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = applySTGen (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applySTGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applySTGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applySTGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applySTGen genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString n = applySTGen (genShortByteString n)

-- | Applies a pure operation to the wrapped pseudo-random number generator.
--
-- @since 1.2
applySTGen :: (g -> (a, g)) -> STGen g s -> ST s a
applySTGen f (STGenI ref) = do
  g <- readSTRef ref
  case f g of
    (!a, !g') -> a <$ writeSTRef ref g'
{-# INLINE applySTGen #-}

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator.
--
-- @since 1.2
runSTGen :: RandomGen g => g -> (forall s . STGen g s -> ST s a) -> (a, g)
runSTGen g action = unSTGen <$> runST (runGenM (STGen g) action)

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator. Returns only the resulting pseudo-random
-- value.
--
-- @since 1.2
runSTGen_ :: RandomGen g => g -> (forall s . STGen g s -> ST s a) -> a
runSTGen_ g action = fst $ runSTGen g action

instance RandomGen SM.SMGen where
  next = SM.nextInt
  genWord32 = SM.nextWord32
  genWord64 = SM.nextWord64
  split = SM.splitSMGen

instance RandomGen SM32.SMGen where
  next = SM32.nextInt
  genWord32 = SM32.nextWord32
  genWord64 = SM32.nextWord64
  split = SM32.splitSMGen

deriving instance RandomGen StdGen

-- | Constructs a 'StdGen' deterministically.
mkStdGen :: Int -> StdGen
mkStdGen = StdGen . SM.mkSMGen . fromIntegral


-- $uniform
-- This library provides two type classes to generate pseudo-random values:
--
-- *   'UniformRange' is used to generate a value of a datatype uniformly
--     within an inclusive range.
-- *   'Uniform' is used to generate a value of a datatype uniformly over all
--     possible values of that datatype.
--
-- Types may have instances for both or just one of 'UniformRange' and
-- 'Uniform'. A few examples illustrate this:
--
-- *   'Int', 'Word16' and 'Bool' are instances of both 'UniformRange' and
--     'Uniform'.
-- *   'Integer', 'Float' and 'Double' each have an instance for 'UniformRange'
--     but no 'Uniform' instance.
-- *   A hypothetical type @Radian@ representing angles by taking values in the
--     range @[0, 2π)@ has a trivial 'Uniform' instance, but no 'UniformRange'
--     instance: the problem is that two given @Radian@ values always span /two/
--     ranges, one clockwise and one anti-clockwise.
-- *   It is trivial to construct a @Uniform (a, b)@ instance given
--     @Uniform a@ and @Uniform b@ (and this library provides this tuple
--     instance).
-- *   On the other hand, there is no correct way to construct a
--     @UniformRange (a, b)@ instance based on just @UniformRange a@ and
--     @UniformRange b@.


-- | Generates a value uniformly distributed over all possible values of that
-- datatype.
--
-- @since 1.2
class Uniform a where
  uniformM :: MonadRandom g s m => g s -> m a

-- | Generates a value uniformly distributed over the provided inclusive range.
--
-- For example, @uniformR (1,4)@ should generate values uniformly from the set
-- @[1,2,3,4]@.
--
-- The API uses an inclusive range so any range can be expressed, even when
-- using fixed-size ints, enumerations etc.
--
-- The following law should hold to make the function always defined:
--
-- > uniformRM (a,b) = uniformM (b,a)
--
-- @since 1.2
class UniformRange a where
  uniformRM :: MonadRandom g s m => (a, a) -> g s -> m a

{- |
With a source of pseudo-random number supply in hand, the 'Random' class allows
the programmer to extract pseudo-random values of a variety of types.

Minimal complete definition: 'randomR' and 'random'.

-}
{-# DEPRECATED randomRIO "In favor of `uniformRM`" #-}
{-# DEPRECATED randomIO "In favor of `uniformRM`" #-}
class Random a where

  -- | Takes a range /(lo,hi)/ and a pseudo-random number generator
  -- /g/, and returns a pseudo-random value uniformly distributed over the
  -- closed interval /[lo,hi]/, together with a new generator. It is unspecified
  -- what happens if /lo>hi/. For continuous types there is no requirement
  -- that the values /lo/ and /hi/ are ever produced, but they may be,
  -- depending on the implementation and the interval.
  {-# INLINE randomR #-}
  randomR :: RandomGen g => (a, a) -> g -> (a, g)
  default randomR :: (RandomGen g, UniformRange a) => (a, a) -> g -> (a, g)
  randomR r g = runGenState g (uniformRM r)

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
  default random :: (RandomGen g, Uniform a) => g -> (a, g)
  random g = runGenState g uniformM

  -- | Plural variant of 'randomR', producing an infinite list of
  -- pseudo-random values instead of returning a new generator.
  {-# INLINE randomRs #-}
  randomRs :: RandomGen g => (a,a) -> g -> [a]
  randomRs ival g = build (\cons _nil -> buildRandoms cons (randomR ival) g)

  -- | Plural variant of 'random', producing an infinite list of
  -- pseudo-random values instead of returning a new generator.
  {-# INLINE randoms #-}
  randoms  :: RandomGen g => g -> [a]
  randoms  g      = build (\cons _nil -> buildRandoms cons random g)

  -- | A variant of 'randomR' that uses the global pseudo-random number
  -- generator.
  randomRIO :: (a,a) -> IO a
  randomRIO range  = getStdRandom (randomR range)

  -- | A variant of 'random' that uses the global pseudo-random number
  -- generator.
  randomIO  :: IO a
  randomIO   = getStdRandom random

-- | Produce an infinite list-equivalent of pseudo-random values.
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

-- Generate values in the Int range
instance Random Integer where
  random = first (toInteger :: Int -> Integer) . random

instance UniformRange Integer where
  uniformRM = uniformIntegerM

instance Random Int8
instance Uniform Int8 where
  uniformM = fmap (fromIntegral :: Word8 -> Int8) . uniformWord8
instance UniformRange Int8 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int8 -> Word8) fromIntegral

instance Random Int16
instance Uniform Int16 where
  uniformM = fmap (fromIntegral :: Word16 -> Int16) . uniformWord16
instance UniformRange Int16 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int16 -> Word16) fromIntegral

instance Random Int32
instance Uniform Int32 where
  uniformM = fmap (fromIntegral :: Word32 -> Int32) . uniformWord32
instance UniformRange Int32 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int32 -> Word32) fromIntegral

instance Random Int64
instance Uniform Int64 where
  uniformM = fmap (fromIntegral :: Word64 -> Int64) . uniformWord64
instance UniformRange Int64 where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int64 -> Word64) fromIntegral

instance Random Int
instance Uniform Int where
#if WORD_SIZE_IN_BITS < 64
  uniformM = fmap (fromIntegral :: Word32 -> Int) . uniformWord32
#else
  uniformM = fmap (fromIntegral :: Word64 -> Int) . uniformWord64
#endif
  {-# INLINE uniformM #-}
instance UniformRange Int where
  uniformRM = signedBitmaskWithRejectionRM (fromIntegral :: Int -> Word) fromIntegral
  {-# INLINE uniformRM #-}

instance Random Word
instance Uniform Word where
#if WORD_SIZE_IN_BITS < 64
  uniformM = fmap (fromIntegral :: Word32 -> Word) . uniformWord32
#else
  uniformM = fmap (fromIntegral :: Word64 -> Word) . uniformWord64
#endif
instance UniformRange Word where
  {-# INLINE uniformRM #-}
  uniformRM = unsignedBitmaskWithRejectionRM

instance Random Word8
instance Uniform Word8 where
  {-# INLINE uniformM #-}
  uniformM = uniformWord8
instance UniformRange Word8 where
  {-# INLINE uniformRM #-}
  uniformRM = unsignedBitmaskWithRejectionRM

instance Random Word16
instance Uniform Word16 where
  {-# INLINE uniformM #-}
  uniformM = uniformWord16
instance UniformRange Word16 where
  {-# INLINE uniformRM #-}
  uniformRM = unsignedBitmaskWithRejectionRM

instance Random Word32
instance Uniform Word32 where
  {-# INLINE uniformM #-}
  uniformM  = uniformWord32
instance UniformRange Word32 where
  {-# INLINE uniformRM #-}
  uniformRM (b, t) g | b > t     = (+t) <$> unbiasedWordMult32 (b - t) g
                    | otherwise = (+b) <$> unbiasedWordMult32 (t - b) g

instance Random Word64
instance Uniform Word64 where
  {-# INLINE uniformM #-}
  uniformM  = uniformWord64
instance UniformRange Word64 where
  {-# INLINE uniformRM #-}
  uniformRM = unsignedBitmaskWithRejectionRM

instance Random CBool
instance Uniform CBool where
  uniformM = fmap CBool . uniformM
instance UniformRange CBool where
  uniformRM (CBool b, CBool t) = fmap CBool . uniformRM (b, t)

instance Random CChar
instance Uniform CChar where
  uniformM = fmap CChar . uniformM
instance UniformRange CChar where
  uniformRM (CChar b, CChar t) = fmap CChar . uniformRM (b, t)

instance Random CSChar
instance Uniform CSChar where
  uniformM = fmap CSChar . uniformM
instance UniformRange CSChar where
  uniformRM (CSChar b, CSChar t) = fmap CSChar . uniformRM (b, t)

instance Random CUChar
instance Uniform CUChar where
  uniformM = fmap CUChar . uniformM
instance UniformRange CUChar where
  uniformRM (CUChar b, CUChar t) = fmap CUChar . uniformRM (b, t)

instance Random CShort
instance Uniform CShort where
  uniformM = fmap CShort . uniformM
instance UniformRange CShort where
  uniformRM (CShort b, CShort t) = fmap CShort . uniformRM (b, t)

instance Random CUShort
instance Uniform CUShort where
  uniformM = fmap CUShort . uniformM
instance UniformRange CUShort where
  uniformRM (CUShort b, CUShort t) = fmap CUShort . uniformRM (b, t)

instance Random CInt
instance Uniform CInt where
  uniformM = fmap CInt . uniformM
instance UniformRange CInt where
  uniformRM (CInt b, CInt t) = fmap CInt . uniformRM (b, t)

instance Random CUInt
instance Uniform CUInt where
  uniformM = fmap CUInt . uniformM
instance UniformRange CUInt where
  uniformRM (CUInt b, CUInt t) = fmap CUInt . uniformRM (b, t)

instance Random CLong
instance Uniform CLong where
  uniformM = fmap CLong . uniformM
instance UniformRange CLong where
  uniformRM (CLong b, CLong t) = fmap CLong . uniformRM (b, t)

instance Random CULong
instance Uniform CULong where
  uniformM = fmap CULong . uniformM
instance UniformRange CULong where
  uniformRM (CULong b, CULong t) = fmap CULong . uniformRM (b, t)

instance Random CPtrdiff
instance Uniform CPtrdiff where
  uniformM = fmap CPtrdiff . uniformM
instance UniformRange CPtrdiff where
  uniformRM (CPtrdiff b, CPtrdiff t) = fmap CPtrdiff . uniformRM (b, t)

instance Random CSize
instance Uniform CSize where
  uniformM = fmap CSize . uniformM
instance UniformRange CSize where
  uniformRM (CSize b, CSize t) = fmap CSize . uniformRM (b, t)

instance Random CWchar
instance Uniform CWchar where
  uniformM = fmap CWchar . uniformM
instance UniformRange CWchar where
  uniformRM (CWchar b, CWchar t) = fmap CWchar . uniformRM (b, t)

instance Random CSigAtomic
instance Uniform CSigAtomic where
  uniformM = fmap CSigAtomic . uniformM
instance UniformRange CSigAtomic where
  uniformRM (CSigAtomic b, CSigAtomic t) = fmap CSigAtomic . uniformRM (b, t)

instance Random CLLong
instance Uniform CLLong where
  uniformM = fmap CLLong . uniformM
instance UniformRange CLLong where
  uniformRM (CLLong b, CLLong t) = fmap CLLong . uniformRM (b, t)

instance Random CULLong
instance Uniform CULLong where
  uniformM = fmap CULLong . uniformM
instance UniformRange CULLong where
  uniformRM (CULLong b, CULLong t) = fmap CULLong . uniformRM (b, t)

instance Random CIntPtr
instance Uniform CIntPtr where
  uniformM                         = fmap CIntPtr . uniformM
instance UniformRange CIntPtr where
  uniformRM (CIntPtr b, CIntPtr t) = fmap CIntPtr . uniformRM (b, t)

instance Random CUIntPtr
instance Uniform CUIntPtr where
  uniformM = fmap CUIntPtr . uniformM
instance UniformRange CUIntPtr where
  uniformRM (CUIntPtr b, CUIntPtr t) = fmap CUIntPtr . uniformRM (b, t)

instance Random CIntMax
instance Uniform CIntMax where
  uniformM = fmap CIntMax . uniformM
instance UniformRange CIntMax where
  uniformRM (CIntMax b, CIntMax t) = fmap CIntMax . uniformRM (b, t)

instance Random CUIntMax
instance Uniform CUIntMax where
  uniformM = fmap CUIntMax . uniformM
instance UniformRange CUIntMax where
  uniformRM (CUIntMax b, CUIntMax t) = fmap CUIntMax . uniformRM (b, t)

instance Random CFloat where
  randomR (CFloat l, CFloat h) = first CFloat . randomR (l, h)
  random = first CFloat . random
instance UniformRange CFloat where
  uniformRM (CFloat l, CFloat h) = fmap CFloat . uniformRM (l, h)

instance Random CDouble where
  randomR (CDouble l, CDouble h) = first CDouble . randomR (l, h)
  random = first CDouble . random
instance UniformRange CDouble where
  uniformRM (CDouble l, CDouble h) = fmap CDouble . uniformRM (l, h)


-- The `chr#` and `ord#` are the prim functions that will be called, regardless of which
-- way you gonna do the `Char` conversion, so it is better to call them directly and
-- bypass all the hoops. Also because `intToChar` and `charToInt` are internal functions
-- and are called on valid character ranges it is impossible to generate an invalid
-- `Char`, therefore it is totally fine to omit all the unnecessary checks involved in
-- other paths of conversion.
word32ToChar :: Word32 -> Char
word32ToChar (W32# w#) = C# (chr# (word2Int# w#))
{-# INLINE word32ToChar #-}

charToWord32 :: Char -> Word32
charToWord32 (C# c#) = W32# (int2Word# (ord# c#))
{-# INLINE charToWord32 #-}

instance Random Char
instance Uniform Char where
  uniformM g = word32ToChar <$> unsignedBitmaskWithRejectionM uniformM (charToWord32 maxBound) g
  {-# INLINE uniformM #-}
instance UniformRange Char where
  uniformRM (l, h) g =
    word32ToChar <$> unsignedBitmaskWithRejectionRM (charToWord32 l, charToWord32 h) g
  {-# INLINE uniformRM #-}

instance Random Bool
instance Uniform Bool where
  uniformM = fmap wordToBool . uniformWord8
    where wordToBool w = (w .&. 1) /= 0
instance UniformRange Bool where
  uniformRM (False, False) _g = return False
  uniformRM (True, True)   _g = return True
  uniformRM _               g = uniformM g

instance Random Double where
  randomR r g = runGenState g (uniformRM r)
  random g = runGenState g (uniformRM (0, 1))

instance UniformRange Double where
  uniformRM (l, h) g = do
    w64 <- uniformWord64 g
    let x = word64ToDoubleInUnitInterval w64
    return $ (h - l) * x + l

-- | Turns a given uniformly distributed 'Word64' value into a uniformly
-- distributed 'Double' value in the range [0, 1).
word64ToDoubleInUnitInterval :: Word64 -> Double
word64ToDoubleInUnitInterval w64 = between1and2 - 1.0
  where
    between1and2 = castWord64ToDouble $ (w64 `unsafeShiftR` 12) .|. 0x3ff0000000000000
{-# INLINE word64ToDoubleInUnitInterval #-}

-- | These are now in 'GHC.Float' but unpatched in some versions so
-- for now we roll our own. See
-- https://gitlab.haskell.org/ghc/ghc/-/blob/6d172e63f3dd3590b0a57371efb8f924f1fcdf05/libraries/base/GHC/Float.hs
{-# INLINE castWord32ToFloat #-}
castWord32ToFloat :: Word32 -> Float
castWord32ToFloat (W32# w#) = F# (stgWord32ToFloat w#)

foreign import prim "stg_word32ToFloatyg"
    stgWord32ToFloat :: Word# -> Float#

{-# INLINE castWord64ToDouble #-}
castWord64ToDouble :: Word64 -> Double
castWord64ToDouble (W64# w) = D# (stgWord64ToDouble w)

foreign import prim "stg_word64ToDoubleyg"
#if WORD_SIZE_IN_BITS == 64
    stgWord64ToDouble :: Word# -> Double#
#else
    stgWord64ToDouble :: Word64# -> Double#
#endif


instance Random Float where
  randomR r g = runGenState g (uniformRM r)
  random g = runGenState g (uniformRM (0, 1))
instance UniformRange Float where
  uniformRM (l, h) g = do
    w32 <- uniformWord32 g
    let x = word32ToFloatInUnitInterval w32
    return $ (h - l) * x + l

-- | Turns a given uniformly distributed 'Word32' value into a uniformly
-- distributed 'Float' value in the range [0,1).
word32ToFloatInUnitInterval :: Word32 -> Float
word32ToFloatInUnitInterval w32 = between1and2 - 1.0
  where
    between1and2 = castWord32ToFloat $ (w32 `unsafeShiftR` 9) .|. 0x3f800000
{-# INLINE word32ToFloatInUnitInterval #-}

-- The two integer functions below take an [inclusive,inclusive] range.
randomIvalIntegral :: (RandomGen g, Integral a) => (a, a) -> g -> (a, g)
randomIvalIntegral (l,h) = randomIvalInteger (toInteger l, toInteger h)

{-# SPECIALIZE randomIvalInteger :: (Num a) =>
    (Integer, Integer) -> StdGen -> (a, StdGen) #-}

randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case (f 1 0 rng) of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       (genlo, genhi) = genRange rng
       b = fromIntegral genhi - fromIntegral genlo + 1

       -- Probabilities of the most likely and least likely result
       -- will differ at most by a factor of (1 +- 1/q). Assuming the RandomGen
       -- is uniform, of course

       -- On average, log q / log b more pseudo-random values will be generated
       -- than the minimum
       q = 1000
       k = h - l + 1
       magtgt = k * q

       -- generate pseudo-random values until we exceed the target magnitude
       f mag v g | mag >= magtgt = (v, g)
                 | otherwise = v' `seq`f (mag*b) v' g' where
                        (x,g') = next g
                        v' = (v * b + (fromIntegral x - fromIntegral genlo))

-- | Generate an 'Integer' in the range @[l, h]@ if @l <= h@ and @[h, l]@
-- otherwise.
uniformIntegerM :: (MonadRandom g s m) => (Integer, Integer) -> g s -> m Integer
uniformIntegerM (l, h) gen = case l `compare` h of
  LT -> do
    let limit = h - l
    let limitAsWord64 :: Word64 = fromIntegral limit
    bounded <-
      if (toInteger limitAsWord64) == limit
        -- Optimisation: if 'limit' fits into 'Word64', generate a bounded
        -- 'Word64' and then convert to 'Integer'
        then toInteger <$> unsignedBitmaskWithRejectionM uniformWord64 limitAsWord64 gen
        else boundedExclusiveIntegerM (limit + 1) gen
    return $ l + bounded
  GT -> uniformIntegerM (h, l) gen
  EQ -> pure l
{-# INLINE uniformIntegerM #-}

-- | Generate an 'Integer' in the range @[0, s)@ using a variant of Lemire's
-- multiplication method.
--
-- Daniel Lemire. 2019. Fast Random Integer Generation in an Interval. In ACM
-- Transactions on Modeling and Computer Simulation
-- https://doi.org/10.1145/3230636
--
-- PRECONDITION (unchecked): s > 0
boundedExclusiveIntegerM :: (MonadRandom g s m) => Integer -> g s -> m Integer
boundedExclusiveIntegerM s gen = go
  where
    n = integerWordSize s
    -- We renamed 'L' from the paper to 'k' here because 'L' is not a valid
    -- variable name in Haskell and 'l' is already used in the algorithm.
    k = WORD_SIZE_IN_BITS * n
    twoToK = (1::Integer) `shiftL` k
    modTwoToKMask = twoToK - 1

    t = (twoToK - s) `mod` s
    go = do
      x <- uniformIntegerWords n gen
      let m = x * s
      -- m .&. modTwoToKMask == m `mod` twoToK
      let l = m .&. modTwoToKMask
      if l < t
        then go
        -- m `shiftR` k == m `quot` twoToK
        else return $ m `shiftR` k
{-# INLINE boundedExclusiveIntegerM #-}

-- | @integerWordSize i@ returns that least @w@ such that
-- @i <= WORD_SIZE_IN_BITS^w@.
integerWordSize :: Integer -> Int
integerWordSize = go 0
  where
    go !acc i
      | i == 0 = acc
      | otherwise = go (acc + 1) (i `shiftR` WORD_SIZE_IN_BITS)
{-# INLINE integerWordSize #-}

-- | @uniformIntegerWords n@ is a uniformly pseudo-random 'Integer' in the range
-- @[0, WORD_SIZE_IN_BITS^n)@.
uniformIntegerWords :: (MonadRandom g s m) => Int -> g s -> m Integer
uniformIntegerWords n gen = go 0 n
  where
    go !acc i
      | i == 0 = return acc
      | otherwise = do
        (w :: Word) <- uniformM gen
        go ((acc `shiftL` WORD_SIZE_IN_BITS) .|. (fromIntegral w)) (i - 1)
{-# INLINE uniformIntegerWords #-}

-- | Uniformly generate Word32 in @[0, s]@.
unbiasedWordMult32 :: MonadRandom g s m => Word32 -> g s -> m Word32
unbiasedWordMult32 s g
  | s == maxBound = uniformWord32 g
  | otherwise = unbiasedWordMult32Exclusive (s+1) g
{-# INLINE unbiasedWordMult32 #-}

-- | See [Lemire's paper](https://arxiv.org/pdf/1805.10941.pdf),
-- [O\'Neill's
-- blogpost](https://www.pcg-random.org/posts/bounded-rands.html) and
-- more directly [O\'Neill's github
-- repo](https://github.com/imneme/bounded-rands/blob/3d71f53c975b1e5b29f2f3b05a74e26dab9c3d84/bounded32.cpp#L234).
-- N.B. The range is [0,t) **not** [0,t].
unbiasedWordMult32Exclusive  :: MonadRandom g s m => Word32 -> g s -> m Word32
unbiasedWordMult32Exclusive r g = go
  where
    t :: Word32
    t = (-r) `mod` r -- Calculates 2^32 `mod` r!!!
    go = do
      x <- uniformWord32 g
      let m :: Word64
          m = (fromIntegral x) * (fromIntegral r)
          l :: Word32
          l = fromIntegral m
      if (l >= t) then return (fromIntegral $ m `shiftR` 32) else go

-- | This only works for unsigned integrals
unsignedBitmaskWithRejectionRM ::
     (MonadRandom g s m, FiniteBits a, Num a, Ord a, Uniform a)
  => (a, a)
  -> g s
  -> m a
unsignedBitmaskWithRejectionRM (bottom, top) gen
  | bottom > top = unsignedBitmaskWithRejectionRM (top, bottom) gen
  | bottom == top = pure top
  | otherwise = (bottom +) <$> unsignedBitmaskWithRejectionM uniformM range gen
  where
    range = top - bottom
{-# INLINE unsignedBitmaskWithRejectionRM #-}

-- | This works for signed integrals by explicit conversion to unsigned and abusing overflow
signedBitmaskWithRejectionRM ::
     (Num a, Num b, Ord b, Ord a, FiniteBits a, MonadRandom g s f, Uniform a)
  => (b -> a)
  -> (a -> b)
  -> (b, b)
  -> g s
  -> f b
signedBitmaskWithRejectionRM toUnsigned fromUnsigned (bottom, top) gen
  | bottom > top = signedBitmaskWithRejectionRM toUnsigned fromUnsigned (top, bottom) gen
  | bottom == top = pure top
  | otherwise = (bottom +) . fromUnsigned <$>
    unsignedBitmaskWithRejectionM uniformM range gen
    where
      -- This works in all cases, see Appendix 1 at the end of the file.
      range = toUnsigned top - toUnsigned bottom
{-# INLINE signedBitmaskWithRejectionRM #-}

-------------------------------------------------------------------------------
-- 'Uniform' instances for tuples
-------------------------------------------------------------------------------

instance (Uniform a, Uniform b) => Uniform (a, b) where
  uniformM g = (,) <$> uniformM g <*> uniformM g

instance (Uniform a, Uniform b, Uniform c) => Uniform (a, b, c) where
  uniformM g = (,,) <$> uniformM g <*> uniformM g <*> uniformM g

instance (Uniform a, Uniform b, Uniform c, Uniform d) => Uniform (a, b, c, d) where
  uniformM g = (,,,) <$> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g

instance (Uniform a, Uniform b, Uniform c, Uniform d, Uniform e) => Uniform (a, b, c, d, e) where
  uniformM g = (,,,,) <$> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g

instance (Uniform a, Uniform b, Uniform c, Uniform d, Uniform e, Uniform f) => Uniform (a, b, c, d, e, f) where
  uniformM g = (,,,,,) <$> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g

instance (Uniform a, Uniform b, Uniform c, Uniform d, Uniform e, Uniform f, Uniform g) => Uniform (a, b, c, d, e, f, g) where
  uniformM g = (,,,,,,) <$> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g

-------------------------------------------------------------------------------
-- Global pseudo-random number generator
-------------------------------------------------------------------------------

-- $globalstdgen
--
-- There is a single, implicit, global pseudo-random number generator of type
-- 'StdGen', held in a global variable maintained by the 'IO' monad. It is
-- initialised automatically in some system-dependent fashion. To get
-- deterministic behaviour, use 'setStdGen'.
--
-- Note that 'mkStdGen' also gives deterministic behaviour without requiring an
-- 'IO' context.

-- |Sets the global pseudo-random number generator.
setStdGen :: StdGen -> IO ()
setStdGen sgen = writeIORef theStdGen sgen

-- |Gets the global pseudo-random number generator.
getStdGen :: IO StdGen
getStdGen  = readIORef theStdGen

theStdGen :: IORef StdGen
theStdGen = unsafePerformIO $ SM.initSMGen >>= newIORef . StdGen
{-# NOINLINE theStdGen #-}

-- |Applies 'split' to the current global pseudo-random generator,
-- updates it with one of the results, and returns the other.
newStdGen :: IO StdGen
newStdGen = atomicModifyIORef' theStdGen split

{- |Uses the supplied function to get a value from the current global
random generator, and updates the global generator with the new generator
returned by the function. For example, @rollDice@ gets a pseudo-random integer
between 1 and 6:

>  rollDice :: IO Int
>  rollDice = getStdRandom (randomR (1,6))

-}
getStdRandom :: (StdGen -> (a,StdGen)) -> IO a
getStdRandom f = atomicModifyIORef' theStdGen (swap . f)
  where swap (v,g) = (g,v)

-------------------------------------------------------------------------------
-- Notes
-------------------------------------------------------------------------------

-- $implementrandomgen
--
-- Consider these points when writing a 'RandomGen' instance for a given pure
-- pseudo-random number generator:
--
-- *   If the pseudo-random number generator has a power-of-2 modulus, that is,
--     it natively outputs @2^n@ bits of randomness for some @n@, implement
--     'genWord8', 'genWord16', 'genWord32' and 'genWord64'. See below for more
--     details.
--
-- *   If the pseudo-random number generator does not have a power-of-2
--     modulus, implement 'next' and 'genRange'. See below for more details.
--
-- *   If the pseudo-random number generator is splittable, implement 'split'.
--     If there is no suitable implementation, 'split' should fail with a
--     helpful error message.
--
-- === How to implement 'RandomGen' for a pseudo-random number generator with power-of-2 modulus
--
-- Suppose you want to implement a [permuted congruential
-- generator](https://en.wikipedia.org/wiki/Permuted_congruential_generator).
--
-- >>> data PCGen = PCGen !Word64 !Word64
--
-- It produces a full 'Word32' of randomness per iteration.
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
-- You can make it an instance of 'RandomGen' as follows:
--
-- >>> :{
-- instance RandomGen PCGen where
--   genWord32 = stepGen
--   genWord64 g = (buildWord64 x y, g'')
--     where
--       (x, g') = stepGen g
--       (y, g'') = stepGen g'
-- :}
--
-- This definition satisfies the compiler. However, the default implementations
-- of 'genWord8' and 'genWord16' are geared towards backwards compatibility
-- with 'RandomGen' instances based on 'next' and 'genRange'. This means that
-- they are not optimal for pseudo-random number generators with a power-of-2
-- modulo.
--
-- So let's implement a faster 'RandomGen' instance for our pseudo-random
-- number generator as follows:
--
-- >>> newtype PCGen' = PCGen' { unPCGen :: PCGen }
-- >>> let stepGen' = second PCGen' . stepGen . unPCGen
-- >>> :{
-- instance RandomGen PCGen' where
--   genWord8 = first fromIntegral . stepGen'
--   genWord16 = first fromIntegral . stepGen'
--   genWord32 = stepGen'
--   genWord64 g = (buildWord64 x y, g'')
--     where
--       (x, g') = stepGen' g
--       (y, g'') = stepGen' g'
-- :}
--
-- === How to implement 'RandomGen' for a pseudo-random number generator without a power-of-2 modulus
--
-- __We do not recommend you implement any new pseudo-random number generators without a power-of-2 modulus.__
--
-- Pseudo-random number generators without a power-of-2 modulus perform
-- /significantly worse/ than pseudo-random number generators with a power-of-2
-- modulus with this library. This is because most functionality in this
-- library is based on generating and transforming uniformly pseudo-random
-- machine words, and generating uniformly pseudo-random machine words using a
-- pseudo-random number generator without a power-of-2 modulus is expensive.
--
-- The pseudo-random number generator from
-- <https://dl.acm.org/doi/abs/10.1145/62959.62969 L’Ecuyer (1988)> natively
-- generates an integer value in the range @[1, 2147483562]@. This is the
-- generator used by this library before it was replaced by SplitMix in version
-- 1.2.
--
-- >>> data LegacyGen = LegacyGen !Int32 !Int32
-- >>> :{
-- let legacyNext :: LegacyGen -> (Int, LegacyGen)
--     legacyNext (LegacyGen s1 s2) = (fromIntegral z', LegacyGen s1'' s2'') where
--       z' = if z < 1 then z + 2147483562 else z
--       z = s1'' - s2''
--       k = s1 `quot` 53668
--       s1'  = 40014 * (s1 - k * 53668) - k * 12211
--       s1'' = if s1' < 0 then s1' + 2147483563 else s1'
--       k' = s2 `quot` 52774
--       s2' = 40692 * (s2 - k' * 52774) - k' * 3791
--       s2'' = if s2' < 0 then s2' + 2147483399 else s2'
-- :}
--
-- You can make it an instance of 'RandomGen' as follows:
--
-- >>> :{
-- instance RandomGen LegacyGen where
--   next = legacyNext
--   genRange _ = (1, 2147483562)
-- :}
--
-- $implementmonadrandom
--
-- Typically, a monadic pseudo-random number generator has facilities to save
-- and restore its internal state in addition to generating pseudo-random
-- pseudo-random numbers.
--
-- Here is an example instance for the monadic pseudo-random number generator
-- from the @mwc-random@ package:
--
-- > instance (s ~ PrimState m, PrimMonad m) => MonadRandom MWC.Gen s m where
-- >   newtype Frozen MWC.Gen = Frozen { unFrozen :: MWC.Seed }
-- >   thawGen = fmap MWC.restore unFrozen
-- >   freezeGen = fmap Frozen . MWC.save
-- >   uniformWord8 = MWC.uniform
-- >   uniformWord16 = MWC.uniform
-- >   uniformWord32 = MWC.uniform
-- >   uniformWord64 = MWC.uniform
-- >   uniformShortByteString n g = unsafeSTToPrim (genShortByteStringST n (MWC.uniform g))
--
-- $deprecations
--
-- Version 1.2 mostly maintains backwards compatibility with version 1.1. This
-- has a few consequences users should be aware of:
--
-- *   The type class 'Random' is deprecated and only provided for backwards
--     compatibility. New code should use 'Uniform' and 'UniformRange' instead.
--
-- *   The methods 'next' and 'genRange' in 'RandomGen' are deprecated and only
--     provided for backwards compatibility. New instances of 'RandomGen' should
--     implement word-based methods instead. See below for more information
--     about how to write a 'RandomGen' instance.
--
-- *   This library provides instances for 'Random' for some unbounded datatypes
--     for backwards compatibility. For an unbounded datatype, there is no way
--     to generate a value with uniform probability out of its entire domain, so
--     the 'random' implementation for unbounded datatypes actually generates a
--     value based on some fixed range.
--
--     For 'Integer', 'random' generates a value in the 'Int' range. For 'Float'
--     and 'Double', 'random' generates a floating point value in the range @[0,
--     1)@.
--
--     This library does not provide 'Uniform' instances for any unbounded
--     datatypes.
--
-- $reproducibility
--
-- If you have two builds of a particular piece of code against this library,
-- any deterministic function call should give the same result in the two
-- builds if the builds are
--
-- *   compiled against the same major version of this library
-- *   on the same architecture (32-bit or 64-bit)
--
-- $references
--
-- 1. Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014. Fast
-- splittable pseudorandom number generators. In Proceedings of the 2014 ACM
-- International Conference on Object Oriented Programming Systems Languages &
-- Applications (OOPSLA '14). ACM, New York, NY, USA, 453-472. DOI:
-- <https://doi.org/10.1145/2660193.2660195>

-- Appendix 1.
--
-- @top@ and @bottom@ are signed integers of bit width @n@. @toUnsigned@
-- converts a signed integer to an unsigned number of the same bit width @n@.
--
--     range = toUnsigned top - toUnsigned bottom
--
-- This works out correctly thanks to modular arithmetic. Conceptually,
--
--     toUnsigned x | x >= 0 = x
--     toUnsigned x | x <  0 = 2^n + x
--
-- The following combinations are possible:
--
-- 1. @bottom >= 0@ and @top >= 0@
-- 2. @bottom < 0@ and @top >= 0@
-- 3. @bottom < 0@ and @top < 0@
--
-- Note that @bottom >= 0@ and @top < 0@ is impossible because of the
-- invariant @bottom < top@.
--
-- For any signed integer @i@ of width @n@, we have:
--
--     -2^(n-1) <= i <= 2^(n-1) - 1
--
-- Considering each combination in turn, we have
--
-- 1. @bottom >= 0@ and @top >= 0@
--
--     range = (toUnsigned top - toUnsigned bottom) `mod` 2^n
--                 --^ top    >= 0, so toUnsigned top    == top
--                 --^ bottom >= 0, so toUnsigned bottom == bottom
--           = (top - bottom) `mod` 2^n
--                 --^ top <= 2^(n-1) - 1 and bottom >= 0
--                 --^ top - bottom <= 2^(n-1) - 1
--                 --^ 0 < top - bottom <= 2^(n-1) - 1
--           = top - bottom
--
-- 2. @bottom < 0@ and @top >= 0@
--
--     range = (toUnsigned top - toUnsigned bottom) `mod` 2^n
--                 --^ top    >= 0, so toUnsigned top    == top
--                 --^ bottom <  0, so toUnsigned bottom == 2^n + bottom
--           = (top - (2^n + bottom)) `mod` 2^n
--                 --^ summand -2^n cancels out in calculation modulo 2^n
--           = (top - bottom) `mod` 2^n
--                 --^ top <= 2^(n-1) - 1 and bottom >= -2^(n-1)
--                 --^ top - bottom <= (2^(n-1) - 1) - (-2^(n-1)) = 2^n - 1
--                 --^ 0 < top - bottom <= 2^n - 1
--           = top - bottom
--
-- 3. @bottom < 0@ and @top < 0@
--
--     range = (toUnsigned top - toUnsigned bottom) `mod` 2^n
--                 --^ top    < 0, so toUnsigned top    == 2^n + top
--                 --^ bottom < 0, so toUnsigned bottom == 2^n + bottom
--           = ((2^n + top) - (2^n + bottom)) `mod` 2^n
--                 --^ summand 2^n cancels out in calculation modulo 2^n
--           = (top - bottom) `mod` 2^n
--                 --^ top <= -1
--                 --^ bottom >= -2^(n-1)
--                 --^ top - bottom <= -1 - (-2^(n-1)) = 2^(n-1) - 1
--                 --^ 0 < top - bottom <= 2^(n-1) - 1
--           = top - bottom

-- $setup
-- >>> import Control.Arrow (first, second)
-- >>> import Control.Monad (replicateM)
-- >>> import Control.Monad.Primitive
-- >>> import Data.Bits
-- >>> import Data.Int (Int32)
-- >>> import Data.Word (Word8, Word16, Word32, Word64)
-- >>> import System.IO (IOMode(WriteMode), withBinaryFile)
-- >>> import qualified System.Random.MWC as MWC
--
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
--
-- >>> :set -fno-warn-missing-methods
--
-- >>> :{
-- let buildWord64 :: Word32 -> Word32 -> Word64
--     buildWord64 x y = (fromIntegral x `shiftL` 32) .|. fromIntegral y
-- :}
--
-- >>> :{
-- instance (s ~ PrimState m, PrimMonad m) => MonadRandom MWC.Gen s m where
--   newtype Frozen MWC.Gen = Frozen { unFrozen :: MWC.Seed }
--   thawGen = fmap MWC.restore unFrozen
--   freezeGen = fmap Frozen . MWC.save
--   uniformWord8 = MWC.uniform
--   uniformWord16 = MWC.uniform
--   uniformWord32 = MWC.uniform
--   uniformWord64 = MWC.uniform
--   uniformShortByteString n g = unsafeSTToPrim (genShortByteStringST n (MWC.uniform g))
-- :}
--
-- >>> :{
-- let rolls :: MonadRandom g s m => Int -> g s -> m [Word8]
--     rolls n = replicateM n . uniformRM (1, 6)
-- :}
