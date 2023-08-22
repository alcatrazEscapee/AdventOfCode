# Advent of Code 2017

Advent of Code 2017 completed in [Cordy](https://github.com/alcatrazEscapee/cordy), a language of my own invention.

Inputs are expected in a folder `/inputs/day##.txt`. Answers (expected output) is expected in `/expected.txt`. Each day can be ran via `make day=##`. All days can be ran with `make test`.

The entire set runs in just over a minute with the current compiler version of Cordy. Longest running days are day 15 (~16s, arithmetic), day 24 (~10s, memoized optimization problem), and day 5 (~9s, arithmetic). A comparable solution for day 15 in Python runs in ~34s in CPython.