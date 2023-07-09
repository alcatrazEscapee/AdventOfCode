# Advent of Code 2016

Advent of (mostly) Haskell

- Days 16, 18 are implemented in C, as both problems lended themselves to procedural, simple array manipulation, and required performant code.
- Days 05, 14, 17 are implemented in Rust, as they required the use of MD5, of which the Haskell standard library does not include, and I did not want to involve any Haskell build tools or dependencies.

All other problems are solved in Haskell. The entire suite can be ran with `make test`, provided that inputs are placed in the folder `./inputs` with the naming convention `./inputs/dayXX.txt`, and a file of expected output is placed in `./inputs/answers.txt`.

Notable Problems:

- Day 11 - The famed 'Microchips and RTGs' problem. I found a state space representation that did all of the state pruning for free, and as a result this was a fairly standard (if complex) BFS.
- Day 12 - This was the most convoluted solution - Haskell is used to generate a C file which is compiled and with the right compiler and settings (`x86-64 clang 15.0.0 -O3`), it reduces the assembly to a single `mov`!
- Day 23, 25 - The two other assembly problems, being unable to utilize a C compiler to do the heavylifting, these had to be decompiled and reverse engineered, and both these days include an extensive comment with an analysis of the input.

Resources:

- [Learn You A Haskell](http://learnyouahaskell.com/chapters)
- VS Code [Haskell](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell) and [Haskell Lint via GHC](https://marketplace.visualstudio.com/items?itemName=dramforever.vscode-ghc-simple) Extensions.
- [GHC 8.8.0.20190613](https://gitlab.haskell.org/ghc/ghc/-/issues/16415)