# random

[![Build Status](https://secure.travis-ci.org/idontgetoutmuch/random.svg?v1.2-proposal)](http://travis-ci.org/idontgetoutmuch/random)
[![Coverage Status](https://coveralls.io/repos/github/idontgetoutmuch/random/badge.svg?branch=v1.2-proposal)](https://coveralls.io/github/idontgetoutmuch/random?branch=v1.2-proposal)

**Pseudo-random number generation for Haskell.**

This library provides
- an interface for pure and optionally splittable pseudo-random number
  generators: [`RandomGen`][haddock-randomgen]
- an interface for monadic pseudo-random number generators:
  [`MonadRandom`][haddock-monadrandom]
- adapters to use pure pseudo-random number generators in monadic code: [see
  documentation][haddock-adapters]
- a pure pseudo-random number generator implementation that is fast,
  splittable, and passes empirical tests: [`StdGen`][haddock-stdgen]

See [the documentation][haddock] for details.

[haddock]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/index.html
[haddock-adapters]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#g:5
[haddock-monadrandom]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random-Monad.html#t:MonadRandom
[haddock-randomgen]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random.html#t:RandomGen
[haddock-stdgen]: https://htmlpreview.github.io/?https://raw.githubusercontent.com/idontgetoutmuch/random/haddock-preview/doc/System-Random.html#t:StdGen
