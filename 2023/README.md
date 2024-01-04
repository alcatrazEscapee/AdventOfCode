# Advent of Code 2023

This Advent of Code attempting to complete in [Cordy](https://github.com/alcatrazEscapee/cordy), a language of my own invention. Hopefully not running into any compiler bugs or missing features in the way.*

*A total of seven compiler bugs emerged and were quashed throughout the month, plus a number of inspirations for new or expanded features, both in the language and standard library.

Inputs are expected in a folder `./inputs/dayXX.txt`, and a single day can be ran with `make day=XX`. Expected output of running all days is expected in `./inputs/expected.txt`, and can be tested - including runtime - by running `make test`.

- All** days are implemented in Cordy, most days running in < 0.1s, with the longest running days being Day 17 (~3.5s), Day 16, (~2.3s), and Day 14 (~1.5s).
- Day 10, 12 run against all examples in `./inputs/day10_examples.txt` Examples are `\n\n` separated alternating between the example input, and expected answer (as a `(part1, part2)` pair)
- In addition to Cordy, some days are also completed in other languages. These are tested when running `make test`, and all have a < 0.1s runtime:
  - Day 5, 9, 11, 13, 18 are also implemented in Haskell, and can be ran with `make haskell day=XX`.
  - Day 14, 16 are also implemented in C++, and can be ran with `make cpp day=XX`.
  - **Day 24 was implemented in Python - after trying unsuccessfully, this proved not feasible for Cordy, due to a lack of both arbitrary-precision integers, and no floating-point data type.