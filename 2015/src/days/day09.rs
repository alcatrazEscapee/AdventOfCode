use std::collections::HashMap;

use fancy_regex::Regex;
use itertools::Itertools;

use crate::utils;
use crate::utils::CapturesExtension;

// Traveling Salesman Problem, using the most simple of methods: brute force!
// Use of 'permutations' from itertools avoids the need for a BFS or other graph-traversal algorithms.

const INPUT: &str = include_str!("../../inputs/day09.txt");

pub fn both() -> (u32, u32) {
    let line_re: Regex = Regex::new(r"(\w+) to (\w+) = (\d+)").unwrap();
    let mut cities: Vec<&str> = Vec::new();
    let mut distances: HashMap<(usize, usize), u32> = HashMap::new();
    for line in INPUT.lines() {
        let captures = line_re.captures(line).unwrap().unwrap();
        let start: usize = utils::index_or_insert(&mut cities, captures.get_str(1));
        let end: usize = utils::index_or_insert(&mut cities, captures.get_str(2));
        let distance: u32 = captures.get_as::<u32>(3);

        distances.insert((start, end), distance);
        distances.insert((end, start), distance);
    }

    cities.iter()
        .enumerate()
        .map(|(i, _)| i)
        .permutations(cities.len())
        .map(|perm| perm.windows(2)
            .map(|pair| *distances.get(&(pair[0], pair[1])).unwrap()).sum::<u32>())
        .minmax()
        .into_option()
        .unwrap()
}