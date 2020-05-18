# Context

Following @lehins' [performance analysis][analysis] of Haskell pseudo-random number libraries and the [ensuing discussion][analysis-discussion], @lehins, @idontgetoutmuch and @curiousleo with help from @Shimuuar [set out to improve `random`][announcement] as both an interface for and implementation of a pseudo-random number generator for Haskell.

Our goals were to fix #25 (filed in 2015) and #51 (filed in 2018), see "Quality" and "Performance" below.

In the process of tackling these two issues, we addressed a number of other issues too (see "Other issues addressed" below) and added a monadic interface to the library so monadic pseudo-random number generators can be used interchangeably with `random`, see "API changes" below.

This PR is the result of that effort. The changes are considerable. To signal this, we propose to release this as version 1.2 (the previous released version is 1.1, from 2014).

However, the API changes are generally backwards-compatible, see "Compatibility" below.

# Quality (#25)

We created an [environment][quality-repo] for running statistical pseudo-random number generator tests, tested `random` v1.1 and `splitmix` using dieharder, TestU01, PractRand and other test suites and [recorded the results][quality-results].

The results clearly show that the `split` operation in `random` v1.1 produces pseudo-random number generators which are correlated, corroborating #25. The `split` operation in `splitmix` showed no weakness in our tests.

As a result, we replaced the pseudo-random number generator implementation in `random` by the one provided by `splitmix`.

# Performance (#51)

@lehins' [performance analysis][analysis] has the data for `random` v1.1. It is slow, and using faster pseudo-random number generators via `random` v1.1 makes them slow.

By switching to `splitmix` and improving the API, this PR speeds up pseudo-random number generation with `random` by one to three orders of magnitude, depending on the number type. See [Benchmarks][benchmarks] for details.

# API changes

The `Random` typeclass has conceptually been split into [`Uniform`][uniform-docs] and [`UniformRange`][uniformrange-docs]. The `Random` typeclass is still included for backwards compatibility. `Uniform` is for types where it is possible to sample from the type's entire domain; `UniformRange` is for types where one can sample from a specified range.

A new module [`System.Random.Monad`][random-monad] allows monadic pseudo-random number generators like `mwc-random` to be used via the new interface provided by `random`, see the docs for an [example of how this works][mwc-example].

In addition, the module provides a convenient way to [run pure generators in monadic code][pure-gen].

# Compatibility

We strived to make changes backwards compatible where possible and desirable.

The following changes may break existing packages:

- import clashes, e.g. with the new functions `uniform` and `uniformR`
- `randomIO` and `randomRIO` where extracted outside of `Random` class as separate functions, which means some packages need to adjust how they are imported
- `StdGen` is no longer an instance of `Read`
- requires `base >= 4.10` (GHC-8.2)

In addition, `genRange` and `next` have been deprecated.

We have built all of Stackage against the code in this PR, and confirmed that no other build breakages occurred.

For more details, see https://github.com/haskell/random/pull/61#issuecomment-628173793 and [the "Compatibility" section in the docs][compatibility].

# Other issues addressed

This PR also addresses #26, #44, #53, #55, #58 and #59, see [Issues Addressed][issues-addressed] for details.

[analysis]: https://alexey.kuleshevi.ch/blog/2019/12/21/random-benchmarks/
[analysis-discussion]: https://www.reddit.com/r/haskell/comments/edr9n4/random_benchmarks/
[announcement]: https://mail.haskell.org/pipermail/libraries/2020-February/030261.html
[benchmarks]: https://github.com/idontgetoutmuch/random/blob/v1.2-proposal/CHANGELOG.md#benchmarks
[compatibility]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random.html#g:6
[issues-addressed]: https://github.com/idontgetoutmuch/random/blob/v1.2-proposal/CHANGELOG.md#issues-addressed
[mwc-example]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#g:13
[pure-gen]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#g:5
[quality-repo]: https://github.com/tweag/random-quality
[quality-results]: https://github.com/tweag/random-quality/tree/master/results
[random-monad]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html
[uniformrange-docs]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#t:UniformRange
[uniform-docs]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#t:Uniform
