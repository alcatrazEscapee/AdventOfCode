use fancy_regex::{Regex};
use crate::utils::{CapturesExtension, RegexExtension};
use std::collections::HashMap;

const INPUT: &str = include_str!("../../inputs/day16.txt");
const TARGET: &str = "
children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1";

pub fn both() -> (usize, usize) {
    let find_re: Regex = Regex::new(r"([a-z]+: \d+)").unwrap();
    let parse_re: Regex = Regex::new(r"([a-z]+): (\d+)").unwrap();

    let targets: HashMap<&str, u32> = parse(&find_re, &parse_re, TARGET);
    let properties: Vec<HashMap<&str, u32>> = INPUT.lines()
        .map(|line| parse(&find_re, &parse_re, line))
        .collect();

    // For each Sue's properties, find one that matches all rules, either by not having the key, or having an identical value
    let part1: usize = properties.iter()
        .enumerate()
        .filter(|(_, p)| targets.iter()
            .all(|(k, v)| !p.contains_key(k) || *v == p[k]))
        .next()
        .unwrap().0 + 1;

    let part2: usize = properties.iter()
        .enumerate()
        .filter(|(_, p)| targets.iter()
            .all(|(k, v)| !p.contains_key(k) || (
                match *k {
                    "cats" | "trees" => p[k] > *v,
                    "pomeranians" | "goldfish" => p[k] < *v,
                    _ =>  p[k] == *v
                }
            )))
        .next()
        .unwrap().0 + 1;

    (part1, part2)
}

fn parse<'t>(find_re: &Regex, parse_re: &Regex, text: &'t str) -> HashMap<&'t str, u32> {
    find_re.findall(text)
        .iter()
        .map(|sub| {
            let cs = parse_re.captures(sub).unwrap().unwrap();
            (cs.get_str(1), cs.get_as::<u32>(2))
        })
        .collect()
}