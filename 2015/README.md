### Advent of Code 2015

---

My solutions for [Advent of Code](https://adventofcode.com/) 2015 written as an exercise in learning Rust, and writing performant and efficient algorithms, with a goal of < 1s runtime for every solution, with `--release` compile profile.

Inputs and expected answers are required to build as they are included via `include_str!()`. Inputs must be in `/inputs/day##.txt`, and an expected answers csv must be in `/inputs/answers.txt` with one row per day (part1,part2). Test against the expected answers with `cargo test`, and run (benchmark) with `cargo run --release`.