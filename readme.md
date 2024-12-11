# AOC

* `common/`: common libraries and utility functions
* `meta/`: template-related utilities
* `src/`: years and years

### Generating templates

`meta/Generate.hs` provides `mkYear` to do so.
Launch `stack ghci` at project root and call `mkYear <YEAR>`
to generate 25 Haskell files for 25 days under `src/Y<YEAR>`.

A template looks like:

```haskell
module Y2021.D12 () where

import Common

instance AOC 2021 12 where
  type Input 2021 12 = TypeError (Text "Solution to 2021/12 unimplemented")
  parse :: [String] -> Input 2021 12
  parse = undefined

  type Output1 2021 12 = Integer
  part1 :: Input 2021 12 -> Output1 2021 12
  part1 = undefined

  type Output2 2021 12 = Integer
  part2 :: Input 2021 12 -> Output2 2021 12
  part2 = undefined
```

Where:
* `Input` determines the input type of your puzzle
* `parse` parses that from lines of raw input
* `Output1` and `Output2` are output types for part 1 and 2, often (always) integers
* `part1` and `part2` should be self-explanatory

### Adding a solution

You should always generate the templates for a whole year
before attempting problems in that year.

One should put solution to `YYYY/DD` in `Y<YYYY>/D<DD>.hs`
and implement the orphan instance for `AOC` on that day.
Don't forget to add raw input to `resources/<YYYY>/<DD>.txt`
also.

### Run a solution

First `stack build`/`cabal build` 
to make sure all the resource files
are visible to `stack`/`cabal`.

`run` from `common/Common.hs` is the way to run a solution:

```haskell
run :: forall (year :: Nat) (day :: Nat) (part :: Nat) -> RunAOC year day part => IO (OutputOn year day part)
```

Use `run <YYYY> <DD> <PART>` to get solution
to the problem on year `YYYY`, day `DD`, part `PART`:

```
>>> run 2021 1 2  -- year 2021, day 1, part 2
1543
```
