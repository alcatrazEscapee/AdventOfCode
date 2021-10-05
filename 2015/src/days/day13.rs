use std::collections::HashMap;

use fancy_regex::Regex;
use itertools::Itertools;

use crate::utils;
use crate::utils::CapturesExtension;

const INPUT: &str = include_str!("../../inputs/day13.txt");

pub fn both() -> (i32, i32) {
    let line_re: Regex = Regex::new(r"(\w+) would (lose|gain) (\d+) happiness units by sitting next to (\w+).").unwrap();
    let mut people: Vec<&str> = Vec::new();
    let mut deltas: HashMap<(usize, usize), i32> = HashMap::new();
    for line in INPUT.lines() {
        let captures = line_re.captures(line).unwrap().unwrap();
        let target: usize = utils::index_or_insert(&mut people, captures.get_str(1));
        let adjacent: usize = utils::index_or_insert(&mut people, captures.get_str(4));
        let delta: i32 = if captures.get_str(2) == "gain" { 1 } else { -1 } * captures.get_as::<i32>(3);

        deltas.insert((target, adjacent), delta);
    }

    let part1: i32 = solve(&deltas, &people);
    people.push("You");
    let part2: i32 = solve(&deltas, &people);
    (part1, part2)
}

fn solve(deltas: &HashMap<(usize, usize), i32>, people: &Vec<&str>) -> i32 {
    #[inline]
    fn pair_delta(deltas: &HashMap<(usize, usize), i32>, pair: &[usize]) -> i32 {
        deltas.get(&(*pair.get(0).unwrap(), *pair.get(1).unwrap())).unwrap_or(&0) +
            deltas.get(&(*pair.get(1).unwrap(), *pair.get(0).unwrap())).unwrap_or(&0)
    }

    people.iter()
        .enumerate()
        .map(|(i, _)| i)
        .permutations(people.len())
        .map(|mut perm| {
            perm.push(*perm.get(0).unwrap());
            perm.windows(2)
                .map(|pair| pair_delta(&deltas, pair))
                .sum::<i32>()
        })
        .max()
        .unwrap()
}