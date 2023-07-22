# Advent of Code 2018

Advent of Code 2018 completed in C / C++. I strived to do as many days in pure C as possible, but allowed myself to use C++ in cases of particularly difficult parsing, or requiring a lot of complex data structures. I originally completed this year in Python, but then went back to make (1) robust, (2) fast, and clean solutions in C / C++, and also as an exercise to improve my knowledge of the language(s). `lib` contains a few utilities (homegrown data structures for C, and a re-used `Point` class for C++) used by multiple days.

Inputs are expected in a folder `/inputs/dayXX.txt`, and an expected output at `/inputs/answers.txt`. Running `make day=XX` will execute a single day, or `make test` will execute all days, and test against the expected answers. `make bench day=XX` runs a benchmark against a particular day (runs 100x times, takes a mean and standard deviation).

All solutions run in under ~0.5 s, most under 0.1 s; the entire `make test` takes ~ 2.5 seconds.

