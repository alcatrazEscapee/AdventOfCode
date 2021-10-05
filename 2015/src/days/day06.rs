use fancy_regex::Regex;
use bit_set::BitSet;
use crate::utils::CapturesExtension;

const INPUT: &str = include_str!("../../inputs/day06.txt");

pub fn both() -> (usize, usize) {
    let mut part1: BitSet = BitSet::with_capacity(1024 * 1024); // Actual range is 1000x1000, round up to allow bitwise operations for index calculations
    let mut part2: Vec<usize> = vec![0; 1024 * 1024];

    for action in parse() {
        for x in action.x0..=action.x1 {
            for y in action.y0..=action.y1 {
                let index: usize = x | (y << 10);
                match action.action {
                    ActionType::Toggle => {
                        match part1.contains(index) {
                            true => part1.remove(index),
                            false => part1.insert(index)
                        };
                        part2[index] += 2;
                    },
                    ActionType::TurnOn => {
                        part1.insert(index);
                        part2[index] += 1;
                    },
                    ActionType::TurnOff => {
                        part1.remove(index);
                        if part2[index] > 0 {
                            part2[index] -= 1;
                        }
                    },
                };
            }
        }
    }
    (part1.len(), part2.iter().sum())
}

enum ActionType {
    Toggle,
    TurnOn,
    TurnOff
}

struct Action {
    action: ActionType,
    x0: usize,
    y0: usize,
    x1: usize,
    y1: usize,
}

impl Action {
    fn new(action: ActionType, x0: usize, y0: usize, x1: usize, y1: usize) -> Action {
        Action {
            action,
            x0,
            y0,
            x1,
            y1
        }
    }
}

fn parse() -> impl Iterator<Item=Action> {
    let line_re = Regex::new(r"(toggle|turn on|turn off) (\d+),(\d+) through (\d+),(\d+)").unwrap();
    INPUT.lines()
        .map(move |line| {
            let captures = line_re.captures(line).unwrap().unwrap();
            let action: ActionType = match captures.get(1).unwrap().as_str() {
                "toggle" => ActionType::Toggle,
                "turn on" => ActionType::TurnOn,
                "turn off" => ActionType::TurnOff,
                other => panic!("Capture 1 matched a weird value {}", other)
            };
            let x0: usize = captures.get_as::<usize>(2);
            let y0: usize = captures.get_as::<usize>(3);
            let x1: usize = captures.get_as::<usize>(4);
            let y1: usize = captures.get_as::<usize>(5);
            Action::new(action, x0, y0, x1, y1)
        })
}