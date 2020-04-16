-- |
-- Module      :  System.Random.Monad
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
--
-- Interfaces and helpers for pseudo-random number generation.
module System.Random.Monad
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

import System.Random.Internal

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
--
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
--
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
--
-- $globalstdgen
--
-- There is a single, implicit, global pseudo-random number generator of type
-- 'StdGen', held in a global variable maintained by the 'IO' monad. It is
-- initialised automatically in some system-dependent fashion. To get
-- deterministic behaviour, use 'setStdGen'.
--
-- Note that 'mkStdGen' also gives deterministic behaviour without requiring an
-- 'IO' context.
--
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
-- >>> import System.Random.Internal
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
