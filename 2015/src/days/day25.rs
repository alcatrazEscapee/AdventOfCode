use fancy_regex::Regex;
use itertools::Itertools;

use crate::utils::RegexExtension;

const INPUT: &'static str = include_str!("../../inputs/day25.txt");

pub fn part1() -> u64 {
    let (row, col) = Regex::new(r"(\d+)").unwrap()
        .findall(INPUT)
        .into_iter()
        .map(|u| u.parse::<u32>().unwrap())
        .collect_tuple()
        .unwrap();
    let mut code: u64 = 20151125;
    for _ in 1..ord(row, col) {
        code = (code * 252533) % 33554393;
    }
    code
}

fn ord(r: u32, c: u32) -> u32 {
    // Denote by ord(r, c) the ordinal at row r, column c
    // -> ord(1, c) is the Nth triangle number = c * (c + 1) / 2
    // -> ord(r, c) = ord(1, c + r - 1) - r + 1
    (c + r) * (c + r - 1) / 2 - r + 1
}