## Advent of Code 2022

---

My Advent of Code solutions from 2022. Most of these are cleaned up and documented from their original competitive speed programming state. The project has no dependencies. Inputs are expected in a folder `./inputs/day##.txt`, and running `main.py` will check each day against a csv file of `/answers.txt`, of the form `part1,part2` for each day (plus one header row). Below are some miscellaneous notes, statistics, and reflections about the event this year:


- Language Used: Python 3.10
- IDE: PyCharm
- Runtime: 59.4 seconds for all 25 days
  - Day 16 (34.9s), Day 23 (14.0s), Day 19 (2.7s), and Day 24 (2.1s) are the slowest, the rest are all less than a second.
- Leaderboard Appearances: 20 (13 part 1's, 7 part 2's)
- Total Leaderboard Score: 1170 points
- Global Leaderboard Rank: 47th
- Best Leaderboard Rank: A tie between Day 18, Part 1 (00:01:43, 10th), and Day 20, Part 2: (00:10:42, 10th). Both days were the result of execution going about as well as it could go... and fast.
- Longest Puzzle: Day 22 (Part 2), at 10:57:22. I spend about two hours on Part 2, before going to bed and re-attempting it in the morning, with an improved mindset and spent most of the day implementing a generic cube folder, which was great fun!
  - Largest Part 2 Difficulty Spike: Day 22 - for the aforementioned reason.
- Favourite puzzle: It is honestly hard to pick... I really enjoyed this AoC, because so many of the puzzles felt novel and exciting. There were of course the "standard computer science algorithm" days, but I felt like I solved some new things, and learned a little along the way! My highlights were:
  - Learning how to write a cube folder (Day 22),
  - Figuring out coordinate transforms and rectangular area subdivisions (Day 15),
  - Seeing one of my [favourite puzzles](https://github.com/alcatrazEscapee/AdventOfCode/commit/be39d55eeb5750cf63da603a4c64b09dcd254e5a) from past years again (Day 14),
  - And even a pretty neat VM/assembly implementation (Day 10)!