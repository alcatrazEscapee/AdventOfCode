use std::collections::HashSet;

const INPUT: &str = include_str!("../../inputs/day03.txt");

pub fn part1() -> usize {
    let mut set: HashSet<(i32, i32)> = HashSet::new();
    deliver_presents(INPUT.chars(), &mut set);
    set.len()
}

pub fn part2() -> usize {
    let mut set: HashSet<(i32, i32)> = HashSet::new();
    deliver_presents(INPUT.chars().step_by(2), &mut set);
    deliver_presents(INPUT.chars().skip(1).step_by(2), &mut set);
    set.len()
}

pub fn deliver_presents(chars: impl Iterator<Item=char>, found: &mut HashSet<(i32, i32)>) {
    let mut pos: (i32, i32) = (0, 0);
    found.insert(pos);
    for c in chars {
        pos = match c {
            '>' => (pos.0 + 1, pos.1),
            '<' => (pos.0 - 1, pos.1),
            'v' => (pos.0, pos.1 + 1),
            '^' => (pos.0, pos.1 - 1),
            _ => pos
        };
        found.insert(pos);
    }
}