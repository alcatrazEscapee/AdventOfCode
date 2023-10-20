# Advent of Code 2017

Advent of Code 2017 completed in [Cordy](https://github.com/alcatrazEscapee/cordy), a language of my own invention.

Inputs are expected in a folder `/inputs/day##.txt`. Answers (expected output) is expected in `/expected.txt`. Each day can be ran via `make day=##`. All days can be ran with `make test`.

The entire set runs in just under a minute with the current compiler version of Cordy. Longest running days are day 24 (~10s, memoized optimization problem), and day 5 (~9s, arithmetic).

Note that day 15 - previously the longest running day - uses Cordy's native FFI to implement the solution in C (<1s runtime), which was previously at ~15s when in pure Cordy. A comparable solution for day 15 in Python runs in ~34s in CPython.