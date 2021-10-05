
use fancy_regex::Regex;

const INPUT: &str = include_str!("../../inputs/day05.txt");

pub fn part1() -> usize {
    let re_three_vowels: Regex = Regex::new(r"([aeiou].*){3}").unwrap();
    let re_pair: Regex = Regex::new(r"(\w)\1").unwrap();
    let re_naughty_sequences: Regex = Regex::new(r"(ab|cd|pq|zy)").unwrap();
    INPUT.lines()
        .filter(|line| re_three_vowels.is_match(line).unwrap() && re_pair.is_match(line).unwrap() && !re_naughty_sequences.is_match(line).unwrap())
        .count()
}

pub fn part2() -> usize {
    let re_two_ab: Regex = Regex::new(r"(\w)(\w).*\1\2").unwrap();
    let re_aba: Regex = Regex::new(r"(\w)(\w)\1").unwrap();
    INPUT.lines()
        .filter(|line| re_two_ab.is_match(line).unwrap() && re_aba.is_match(line).unwrap())
        .count()
}