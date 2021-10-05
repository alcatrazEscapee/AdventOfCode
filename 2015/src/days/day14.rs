use fancy_regex::Regex;
use itertools::Itertools;

use crate::utils::{CapturesExtension, vec_of};

const INPUT: &str = include_str!("../../inputs/day14.txt");
const DURATION: u32 = 2503;

pub fn both() -> (u32, u32) {
    let line_re: Regex = Regex::new(r"\w+ can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.").unwrap();
    let stats: Vec<(u32, u32, u32)> = INPUT.lines()
        .map(|line| {
            let cs = line_re.captures(line).unwrap().unwrap();
            (cs.get_as::<u32>(1), cs.get_as::<u32>(2), cs.get_as::<u32>(3))
        })
        .collect();

    #[inline]
    fn dist(v: &u32, vt: &u32, rt: &u32, t: u32) -> u32 {
        // Calculate the total number of seconds in flight, then multiply by speed
        // First part vt * (t / (vt + rt) is full periods spent flying, in seconds
        // Second part min(vt, t % (vt + rt)) is the number of seconds into the current period spent flying
        // This is modular arithmetic mod (vt + rt)
        v * (vt * (t / (vt + rt)) + std::cmp::min(vt, &(t % (vt + rt))))
    }

    let part1: u32 = stats.iter()
        .map(|(v, vt, rt)| dist(v, vt, rt, DURATION))
        .max()
        .unwrap();

    let mut points: Vec<u32> = vec_of(stats.len(), 0); // zeros
    for t in 1..=DURATION {
        points[stats.iter()
            .map(|(v, vt, rt)| dist(v, vt, rt, t))
            .position_max().unwrap()] += 1;
    }
    (part1, *points.iter().max().unwrap())
}