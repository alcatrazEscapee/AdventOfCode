use fancy_regex::Regex;
use crate::utils::{RegexExtension, sum_partitions};

const INPUT: &str = include_str!("../../inputs/day15.txt");
const INGREDIENTS: u32 = 100;
const CALORIES: u32 = 500;

pub fn both() -> (u32, u32) {
    let re: Regex = Regex::new(r"(-?\d)").unwrap();
    let values: Vec<Vec<i32>> = INPUT.lines()
        .map(|line| re.findall(line).iter().map(|i| i.parse::<i32>().unwrap()).collect())
        .collect();
    let traits: usize = values.get(0).unwrap().len() - 1;

    let mut part1: u32 = 0;
    let mut part2: u32 = 0;
    for partition in sum_partitions(values.len(), INGREDIENTS) {
        let score: u32 = (0..traits).map(|t| {
            let total: i32 = partition.iter()
                .enumerate()
                .map(|(i, amount)| (*amount as i32) * values[i][t])
                .sum();
            std::cmp::max(0, total) as u32
        }).product();
        let calories: u32 = partition.iter()
            .enumerate()
            .map(|(i, amount)| (*amount as i32) * values[i][traits])
            .sum::<i32>() as u32;
        part1 = std::cmp::max(part1, score);
        if calories == CALORIES {
            part2 = std::cmp::max(part2, score);
        }
    }
    (part1, part2)
}