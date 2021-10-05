use std::collections::{HashSet, HashMap};

const INPUT: &str = include_str!("../../inputs/day18.txt");
const SIZE: i32 = 100;
const CORNERS: [(i32, i32); 4] = [(0, 0), (0, SIZE - 1), (SIZE - 1, 0), (SIZE - 1, SIZE - 1)];

// Split into two unique parse() / parts as trying to manage ownership of the initial state is too much of a pain
pub fn part1() -> usize {
    let mut state: HashSet<(i32, i32)> = parse();
    for _ in 0..SIZE {
        state = advance(&state);
    }
    state.len()
}

pub fn part2() -> usize {
    let mut state: HashSet<(i32, i32)> = parse();
    for _ in 0..SIZE {
        state = advance(&state);
        for corner in CORNERS.iter() {
            state.insert(*corner);
        }
    }
    state.len()
}

fn parse() -> HashSet<(i32, i32)> {
    let mut state: HashSet<(i32, i32)> = HashSet::new();
    for (r, row) in INPUT.lines().enumerate() {
        for (c, col) in row.chars().enumerate() {
            if col == '#' {
                state.insert((r as i32, c as i32));
            }
        }
    }
    state
}

fn advance(state: &HashSet<(i32, i32)>) -> HashSet<(i32, i32)> {
    // Uses an adjacency map, which is computed for every lit position
    // This is more efficient than calculating neighbors for each position
    let mut adjacent: HashMap<(i32, i32), u8> = HashMap::new();
    for (r, c) in state {
        for dr in -1..=1 {
            for dc in -1..=1 {
                if dr != 0 || dc != 0 {
                    let key: (i32, i32) = (r + dr, c + dc);
                    let val: u8 = adjacent.get(&key).unwrap_or(&0) + 1;
                    adjacent.insert(key, val);
                }
            }
        }
    }

    let mut next: HashSet<(i32, i32)> = HashSet::new();
    for r in 0..SIZE {
        for c in 0..SIZE {
            let key: (i32, i32) = (r, c);
            let adj: u8 = *adjacent.get(&key).unwrap_or(&0);
            if adj == 3 || (adj == 2 && state.contains(&key)) {
                next.insert(key);
            }
        }
    }
    next
}