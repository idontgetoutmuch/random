# random v1.2 proposal

[![Build Status](https://secure.travis-ci.org/idontgetoutmuch/random.svg?v1.2-proposal)](http://travis-ci.org/idontgetoutmuch/random)
[![Coverage Status](https://coveralls.io/repos/github/idontgetoutmuch/random/badge.svg?branch=v1.2-proposal)](https://coveralls.io/github/idontgetoutmuch/random?branch=v1.2-proposal)

**This repository contains a proposal for version 1.2 of `random`, a
pseudo-random number library for Haskell.** It is a fork of [version
1.1][upstream-1.1], the most recent [released version][hackage] of this
library. The upstream repository lives at [`haskell/random`][upstream].

[**You can browse the documentation for this proposal here.**][haddock]

[hackage]: https://hackage.haskell.org/package/random
[haddock]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/index.html
[upstream-1.1]: https://github.com/haskell/random/tree/v1.1/
[upstream]: https://github.com/haskell/random/

## Context

Following @lehins' [performance analysis][analysis] of Haskell pseudo-random number libraries and the [ensuing discussion][analysis-discussion], @lehins, @idontgetoutmuch and @curiousleo with help from @Shimuuar [set out to improve `random`][announcement] as both an interface for and implementation of a pseudo-random number generator for Haskell.

Our goals were to fix [#25][issue-25] (filed in 2015) and [#51][issue-51] (filed in 2018), see "Quality" and "Performance" below.

In the process of tackling these two issues, we addressed a number of other issues too (see "Other issues addressed" below) and added a monadic interface to the library so monadic pseudo-random number generators can be used interchangeably with `random`, see "API changes" below.

This proposal is the result of that effort. The changes are considerable. To signal this, we propose to release this as version 1.2 (the previous released version is 1.1, from 2014).

However, the API changes are generally backwards-compatible, see "Compatibility" below.

## Quality ([#25][issue-25])

We created an [environment][quality-repo] for running statistical pseudo-random number generator tests, tested `random` v1.1 and `splitmix` using dieharder, TestU01, PractRand and other test suites and [recorded the results][quality-results].

The results clearly show that the `split` operation in `random` v1.1 produces pseudo-random number generators which are correlated, corroborating [#25][issue-25]. The `split` operation in `splitmix` showed no weakness in our tests.

As a result, we replaced the pseudo-random number generator implementation in `random` by the one provided by `splitmix`.

## Performance ([#51][issue-51])

@lehins' [performance analysis][analysis] has the data for `random` v1.1. It is slow, and using faster pseudo-random number generators via `random` v1.1 makes them slow.

By switching to `splitmix` and improving the API, this proposal speeds up pseudo-random number generation with `random` by one to three orders of magnitude, depending on the number type. See [Benchmarks][benchmarks] for details.

## API changes

### `MonadRandom`

The major API addition in this proposal is the definition of a new class [`MonadRandom`][class-monadrandom]:

```haskell
-- | 'MonadRandom' is an interface to monadic pseudo-random number generators.
class Monad m => MonadRandom g s m | g m -> s where
  {-# MINIMAL freezeGen,thawGen,(uniformWord32|uniformWord64) #-}

  type Frozen g = (f :: Type) | f -> g
  freezeGen :: g s -> m (Frozen g)
  thawGen :: Frozen g -> m (g s)

  uniformWord32 :: g s -> m Word32 -- default implementation in terms of uniformWord64
  uniformWord64 :: g s -> m Word64 -- default implementation in terms of uniformWord32
  -- plus methods for other word sizes and for byte strings
  -- all have default implementations so the MINIMAL pragma holds
```

Conceptually, in `MonadRandom g s m`, `g s` is the type of the generator, `s` is the state type, and `m` the underlying monad. Via the functional dependency `g m -> s`, the state type is determined by the generator and monad.

`Frozen` is the type of the generator's state "at rest". It is defined as an injective type family via `f -> g`, so there is no ambiguity as to which `g` any `Frozen g` belongs to.

This definition is generic enough to accomodate, for example, the [`Gen` type from `mwc-random`][mwc-random-gen], which itself abstracts over the underlying primitive monad and state token. This is the full instance declaration (provided here as an example - this instance is not part of `random` as `random` does not depend on `mwc-random`):

```haskell
instance (s ~ PrimState m, PrimMonad m) => MonadRandom MWC.Gen s m where
  type Frozen MWC.Gen = MWC.Seed
  freezeGen = MWC.save
  thawGen = MWC.restore

  uniformWord8 = MWC.uniform
  uniformWord16 = MWC.uniform
  uniformWord32 = MWC.uniform
  uniformWord64 = MWC.uniform
  uniformShortByteString n g = unsafeSTToPrim (genShortByteStringST n (MWC.uniform g))
```

Four `MonadRandom` instances ("monadic adapters") are provided for pure generators to enable their use in monadic code. The documentation [describes them in detail][pure-gen].

### `Uniform` and `UniformRange`

The `Random` typeclass has conceptually been split into [`Uniform` and `UniformRange`][uniform-vs-uniformrange]. The `Random` typeclass is still included for backwards compatibility. `Uniform` is for types where it is possible to sample from the type's entire domain; `UniformRange` is for types where one can sample from a specified range.

## Changes left out

There were changes we considered and decided against including in this proposal.

Some pseudo-random number generators are splittable, others are not. A good way of communicating this is to have a separate typeclass, `Splittable`, say, which only splittable generators implement. After long discussions (see [this issue][split-issue] and [this PR][split-pr]), we decided against adding `Splittable`: the interface changes would either have been backwards-incompatible or too complex. For now, `split` stays part of the `RandomGen` typeclass. The new documentation suggests that [`split` should call `error`][split-docs] if the generator is not splittable.

Due to floating point rounding, generating a floating point number in a range [can yield surprising results][fp-caveats]. There [are techniques][fp-issue] to generate floating point numbers in a range with actual guarantees, but they are more complex and likely slower than the naive methods, so we decided to postpone this particular issue.

Ranges on the real number line can be inclusive or exclusive in the lower and upper bound. We [considered][clusivity-issue] [API designs][clusivity-pr] that would allow users to communicate precisely what kind of range they wanted to generate. This is particularly relevant for floating point numbers. However, we found that such an API would make more sense in conjunction with an improved method for generating floating point numbers, so we [postponed][clusivity-postponed] this too.

## Compatibility

We strove to make changes backwards compatible where possible and desirable.

The following changes may break existing packages:

- import clashes, e.g. with the new functions `uniform` and `uniformR`
- `randomIO` and `randomRIO` where extracted outside of `Random` class as separate functions, which means some packages need to adjust how they are imported
- `StdGen` is no longer an instance of `Read`
- requires `base >= 4.10` (GHC-8.2)

In addition, `genRange` and `next` have been deprecated.

We have built all of Stackage against the code in this proposal, and confirmed that no other build breakages occurred.

For more details, see [this comment][compatibility-comment] and [the "Compatibility" section in the docs][compatibility].

## Other issues addressed

This proposal also addresses [#26][issue-26], [#44][issue-44], [#53][issue-53], [#55][issue-55], [#58][issue-58] and [#59][issue-59], see [Issues Addressed][issues-addressed] for details.

[analysis]: https://alexey.kuleshevi.ch/blog/2019/12/21/random-benchmarks/
[analysis-discussion]: https://www.reddit.com/r/haskell/comments/edr9n4/random_benchmarks/
[announcement]: https://mail.haskell.org/pipermail/libraries/2020-February/030261.html
[benchmarks]: https://github.com/idontgetoutmuch/random/blob/v1.2-proposal/CHANGELOG.md#benchmarks
[class-monadrandom]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#t:MonadRandom
[class-uniform]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#t:Uniform
[class-uniformrange]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#t:UniformRange
[clusivity-issue]: https://github.com/idontgetoutmuch/random/issues/113
[clusivity-postponed]: https://github.com/idontgetoutmuch/random/issues/113#issuecomment-624041080
[clusivity-pr]: https://github.com/idontgetoutmuch/random/pull/104
[compatibility]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random.html#g:6
[compatibility-comment]: https://github.com/haskell/random/pull/61#issuecomment-628173793
[fp-issue]: https://github.com/idontgetoutmuch/random/issues/105
[fp-caveats]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#g:14
[issue-25]: https://github.com/haskell/random/issues/25
[issue-26]: https://github.com/haskell/random/issues/26
[issue-44]: https://github.com/haskell/random/issues/44
[issue-51]: https://github.com/haskell/random/issues/51
[issue-53]: https://github.com/haskell/random/issues/53
[issue-55]: https://github.com/haskell/random/issues/55
[issue-58]: https://github.com/haskell/random/issues/58
[issue-59]: https://github.com/haskell/random/issues/59
[issues-addressed]: https://github.com/idontgetoutmuch/random/blob/v1.2-proposal/CHANGELOG.md#issues-addressed
[mwc-random-gen]: https://hackage.haskell.org/package/mwc-random-0.14.0.0/docs/System-Random-MWC.html#t:Gen
[pure-gen]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#g:5
[quality-repo]: https://github.com/tweag/random-quality
[quality-results]: https://github.com/tweag/random-quality/tree/master/results
[random-monad]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html
[split-docs]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random.html#v:split
[split-issue]: https://github.com/idontgetoutmuch/random/issues/7
[split-pr]: https://github.com/idontgetoutmuch/random/pull/9
[uniform-vs-uniformrange]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#g:10

