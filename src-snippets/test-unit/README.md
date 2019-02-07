# Unit tests with HSpec and Hedgehog

There are a few ways to write unit tests in Haskell. First there are the typical
tests written with concrete values and assertions - we'll write these tests
using HUnit, one of the few unit testing frameworks. Then there is
property-based, or generative testing. The stereotypical implementation is
QuickCheck, which has many clones in other languages, but we'll be using
Hedgehog which implements the same idea, just a bit differently (there are also
other variants - SmallCheck, LeanCheck, or PropTest). There is also a third
approach, proving code, as implemented in `ghc-proofs`, but that's not yet a
generally useful technique. The test framework that runs these tests (as a tree
of tests) is Tasty.

In `src/`, I've used a small excerpt of code that follows the 'three layer'
principle (setup, effects, business logic/pure layer), and specifically the
second and third layers - `Demo.BusinessLogic` defined a few datatypes, and
`Demo.Effects` defines an effect monad. These are then tested in `test/`.

The rest of the project is fairly standard, except for the `test-suite` section
in `test-unit.cabal`.

```
nix-shell
cabal new-test test-unit:test
```
