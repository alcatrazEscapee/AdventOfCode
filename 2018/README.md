# Advent of Code 2018

Advent of Code 2018 completed in C / C++. I strived to do as many days in pure C as possible, but allowed myself to use C++ in cases of particularly difficult parsing, or requiring a lot of complex data structures. I originally completed this year in Python, but then went back to make (1) robust, (2) fast, and clean solutions in C / C++, and also as an exercise to improve my knowledge of the language(s). `lib` contains a few utilities (homegrown data structures for C, and a re-used `Point` class for C++) used by multiple days.

Inputs are expected in a folder `/inputs/dayXX.txt`, and an expected output at `/inputs/answers.txt`. Running `make day=XX` will execute a single day, or `make test` will execute all days, and test against the expected answers.

All solutions run in well under a second on my machine; the entire `make test` takes ~3 seconds.

