-- |
-- /SplitMix/ is a splittable pseudorandom number generator (PRNG) that is quite fast.
--
-- Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014.
--  Fast splittable pseudorandom number generators. In Proceedings
--  of the 2014 ACM International Conference on Object Oriented
--  Programming Systems Languages & Applications (OOPSLA '14). ACM,
--  New York, NY, USA, 453-472. DOI:
--  <https://doi.org/10.1145/2660193.2660195>
--
--  The paper describes a new algorithm /SplitMix/ for /splittable/
--  pseudorandom number generator that is quite fast: 9 64 bit arithmetic/logical
--  operations per 64 bits generated.
--
--  /SplitMix/ is tested with two standard statistical test suites (DieHarder and
--  TestU01, this implementation only using the former) and it appears to be
--  adequate for "everyday" use, such as Monte Carlo algorithms and randomized
--  data structures where speed is important.
--
--  In particular, it __should not be used for cryptographic or security applications__,
--  because generated sequences of pseudorandom values are too predictable
--  (the mixing functions are easily inverted, and two successive outputs
--  suffice to reconstruct the internal state).
--
--  Note: This module supports all GHCs since GHC-7.0.4,
--  but GHC-7.0 and GHC-7.2 have slow implementation, as there
--  are no native 'popCount'.
--
{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnboxedTuples #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy  #-}
#endif
module System.Random.SplitMix (
    SMGen,
    nextWord64,
    nextWord32,
    splitSMGen,
    -- * Initialisation
    mkSMGen,
    initSMGen,
    newSMGen,
    seedSMGen,
    seedSMGen',
    unseedSMGen,
    ) where

import Control.DeepSeq       (NFData (..))
import Data.Bits             (popCount, shiftL, shiftR, xor, (.|.))
import Data.IORef            (IORef, atomicModifyIORef, newIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word             (Word32, Word64)
import System.IO.Unsafe      (unsafePerformIO)
import Data.Primitive.Types
import GHC.Exts

#if !__GHCJS__
import System.CPUTime (cpuTimePrecision, getCPUTime)
#endif

-- $setup
-- >>> import Text.Read (readMaybe)
-- >>> import Data.List (unfoldr)
-- >>> import Text.Printf (printf)

-------------------------------------------------------------------------------
-- Generator
-------------------------------------------------------------------------------

-- | SplitMix generator state.
data SMGen = SMGen !Word64 !Word64 -- seed and gamma; gamma is odd
  deriving Show

instance NFData SMGen where
    rnf (SMGen _ _) = ()

-- |
--
-- >>> readMaybe "SMGen 1 1" :: Maybe SMGen
-- Just (SMGen 1 1)
--
-- >>> readMaybe "SMGen 1 2" :: Maybe SMGen
-- Nothing
--
-- >>> readMaybe (show (mkSMGen 42)) :: Maybe SMGen
-- Just (SMGen 9297814886316923340 13679457532755275413)
--
instance Read SMGen where
    readsPrec d r =  readParen (d > 10) (\r0 ->
        [ (SMGen seed gamma, r3)
        | ("SMGen", r1) <- lex r0
        , (seed, r2) <- readsPrec 11 r1
        , (gamma, r3) <- readsPrec 11 r2
        , odd gamma
        ]) r

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Generate a 'Word64'.
--
-- >>> take 3 $ map (printf "%x") $ unfoldr (Just . nextWord64) (mkSMGen 1337) :: [String]
-- ["b5c19e300e8b07b3","d600e0e216c0ac76","c54efc3b3cc5af29"]
--
nextWord64 :: SMGen -> (Word64, SMGen)
nextWord64 (SMGen seed gamma) = (mix64 seed', SMGen seed' gamma)
  where
    seed' = seed + gamma

-- | Generate 'Word32' by truncating 'nextWord64'.
--
-- @since 0.0.3
nextWord32 :: SMGen -> (Word32, SMGen)
nextWord32 g = (fromIntegral w64, g') where
    (w64, g') = nextWord64 g

-- | Split a generator into a two uncorrelated generators.
splitSMGen :: SMGen -> (SMGen, SMGen)
splitSMGen (SMGen seed gamma) =
    (SMGen seed'' gamma, SMGen (mix64 seed') (mixGamma seed''))
  where
    seed'  = seed + gamma
    seed'' = seed' + gamma

-------------------------------------------------------------------------------
-- Algorithm
-------------------------------------------------------------------------------

goldenGamma :: Word64
goldenGamma = 0x9e3779b97f4a7c15

-- Note: in JDK implementations the mix64 and mix64variant13
-- (which is inlined into mixGamma) are swapped.
mix64 :: Word64 -> Word64
mix64 z0 =
   -- MurmurHash3Mixer
    let z1 = shiftXorMultiply 33 0xff51afd7ed558ccd z0
        z2 = shiftXorMultiply 33 0xc4ceb9fe1a85ec53 z1
        z3 = shiftXor 33 z2
    in z3

-- used only in mixGamma
mix64variant13 :: Word64 -> Word64
mix64variant13 z0 =
   -- Better Bit Mixing - Improving on MurmurHash3's 64-bit Finalizer
   -- http://zimbry.blogspot.fi/2011/09/better-bit-mixing-improving-on.html
   --
   -- Stafford's Mix13
    let z1 = shiftXorMultiply 30 0xbf58476d1ce4e5b9 z0 -- MurmurHash3 mix constants
        z2 = shiftXorMultiply 27 0x94d049bb133111eb z1
        z3 = shiftXor 31 z2
    in z3

mixGamma :: Word64 -> Word64
mixGamma z0 =
    let z1 = mix64variant13 z0 .|. 1             -- force to be odd
        n  = popCount (z1 `xor` (z1 `shiftR` 1))
    -- see: http://www.pcg-random.org/posts/bugs-in-splitmix.html
    -- let's trust the text of the paper, not the code.
    in if n >= 24
        then z1
        else z1 `xor` 0xaaaaaaaaaaaaaaaa

shiftXor :: Int -> Word64 -> Word64
shiftXor n w = w `xor` (w `shiftR` n)

shiftXorMultiply :: Int -> Word64 -> Word64 -> Word64
shiftXorMultiply n k w = shiftXor n w * k

-------------------------------------------------------------------------------
-- Initialisation
-------------------------------------------------------------------------------

-- | Create 'SMGen' using seed and gamma.
--
-- >>> seedSMGen 2 2
-- SMGen 2 3
--
seedSMGen
    :: Word64 -- ^ seed
    -> Word64 -- ^ gamma
    -> SMGen
seedSMGen seed gamma = SMGen seed (gamma .|. 1)

-- | Like 'seedSMGen' but takes a pair.
seedSMGen' :: (Word64, Word64) -> SMGen
seedSMGen' = uncurry seedSMGen

-- | Extract current state of 'SMGen'.
unseedSMGen :: SMGen -> (Word64, Word64)
unseedSMGen (SMGen seed gamma) = (seed, gamma)

-- | Preferred way to deterministically construct 'SMGen'.
--
-- >>> mkSMGen 42
-- SMGen 9297814886316923340 13679457532755275413
--
mkSMGen :: Word64 -> SMGen
mkSMGen s = SMGen (mix64 s) (mixGamma (s + goldenGamma))

-- | Initialize 'SMGen' using system time.
initSMGen :: IO SMGen
initSMGen = fmap mkSMGen mkSeedTime

-- | Derive a new generator instance from the global 'SMGen' using 'splitSMGen'.
newSMGen :: IO SMGen
newSMGen = atomicModifyIORef theSMGen splitSMGen

theSMGen :: IORef SMGen
theSMGen = unsafePerformIO $ initSMGen >>= newIORef
{-# NOINLINE theSMGen #-}

mkSeedTime :: IO Word64
mkSeedTime = do
    now <- getPOSIXTime
    let lo = truncate now :: Word32
#if __GHCJS__
    let hi = lo
#else
    cpu <- getCPUTime
    let hi = fromIntegral (cpu `div` cpuTimePrecision) :: Word32
#endif
    return $ fromIntegral hi `shiftL` 32 .|. fromIntegral lo

-------------------------------------------------------------------------------
-- System.Random
-------------------------------------------------------------------------------

instance Prim SMGen where
  sizeOf#         = sizeOf128#
  alignment#      = alignment128#
  indexByteArray# = indexByteArray128#
  readByteArray#  = readByteArray128#
  writeByteArray# = writeByteArray128#
  setByteArray#   = setByteArray128#
  indexOffAddr#   = indexOffAddr128#
  readOffAddr#    = readOffAddr128#
  writeOffAddr#   = writeOffAddr128#
  setOffAddr#     = setOffAddr128#
  {-# INLINE sizeOf# #-}
  {-# INLINE alignment# #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray# #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray# #-}
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr# #-}
  {-# INLINE writeOffAddr# #-}
  {-# INLINE setOffAddr# #-}



{-# INLINE sizeOf128# #-}
sizeOf128# :: SMGen -> Int#
sizeOf128# _ = 2# *# sizeOf# (undefined :: Word64)

{-# INLINE alignment128# #-}
alignment128# :: SMGen -> Int#
alignment128# _ = 2# *# alignment# (undefined :: Word64)

{-# INLINE indexByteArray128# #-}
indexByteArray128# :: ByteArray# -> Int# -> SMGen
indexByteArray128# arr# i# =
  let i2# = 2# *# i#
      x = indexByteArray# arr# (i2# +# unInt index1)
      y = indexByteArray# arr# (i2# +# unInt index0)
  in SMGen x y

{-# INLINE readByteArray128# #-}
readByteArray128# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, SMGen #)
readByteArray128# arr# i# =
  \s0 -> case readByteArray# arr# (i2# +# unInt index1) s0 of
    (# s1, x #) -> case readByteArray# arr# (i2# +# unInt index0) s1 of
      (# s2, y #) -> (# s2, SMGen x y #)
  where i2# = 2# *# i#

{-# INLINE writeByteArray128# #-}
writeByteArray128# :: MutableByteArray# s -> Int# -> SMGen -> State# s -> State# s
writeByteArray128# arr# i# (SMGen a b) =
  \s0 -> case writeByteArray# arr# (i2# +# unInt index1) a s0 of
    s1 -> case writeByteArray# arr# (i2# +# unInt index0) b s1 of
      s2 -> s2
  where i2# = 2# *# i#

{-# INLINE setByteArray128# #-}
setByteArray128# :: MutableByteArray# s -> Int# -> Int# -> SMGen -> State# s -> State# s
setByteArray128# = defaultSetByteArray#

{-# INLINE indexOffAddr128# #-}
indexOffAddr128# :: Addr# -> Int# -> SMGen
indexOffAddr128# addr# i# =
  let i2# = 2# *# i#
      x = indexOffAddr# addr# (i2# +# unInt index1)
      y = indexOffAddr# addr# (i2# +# unInt index0)
  in SMGen x y

{-# INLINE readOffAddr128# #-}
readOffAddr128# :: Addr# -> Int# -> State# s -> (# State# s, SMGen #)
readOffAddr128# addr# i# =
  \s0 -> case readOffAddr# addr# (i2# +# unInt index1) s0 of
    (# s1, x #) -> case readOffAddr# addr# (i2# +# unInt index0) s1 of
      (# s2, y #) -> (# s2, SMGen x y #)
  where i2# = 2# *# i#

{-# INLINE writeOffAddr128# #-}
writeOffAddr128# :: Addr# -> Int# -> SMGen -> State# s -> State# s
writeOffAddr128# addr# i# (SMGen a b) =
  \s0 -> case writeOffAddr# addr# (i2# +# unInt index1) a s0 of
    s1 -> case writeOffAddr# addr# (i2# +# unInt index0) b s1 of
      s2 -> s2
  where i2# = 2# *# i#

{-# INLINE setOffAddr128# #-}
setOffAddr128# :: Addr# -> Int# -> Int# -> SMGen -> State# s -> State# s
setOffAddr128# = defaultSetOffAddr#

unInt :: Int -> Int#
unInt (I# i#) = i#

-- Use these indices to get the peek/poke ordering endian correct.
index0, index1 :: Int
#if WORDS_BIGENDIAN
index0 = 1
index1 = 0
#else
index0 = 0
index1 = 1
#endif
