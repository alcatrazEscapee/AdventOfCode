use std::str::FromStr;

use fancy_regex::{Captures, Regex};

// Gets the index of an element in a vector
// If not found, inserts at tail and returns that
pub fn index_or_insert<T: Eq>(vec: &mut Vec<T>, e: T) -> usize {
    for (i, e0) in vec.iter().enumerate() {
        if *e0 == e {
            return i;
        }
    }
    vec.push(e);
    vec.len() - 1
}

// Creates a vector with a given length, filled with the specified value
pub fn vec_of<T>(length: usize, value: T) -> Vec<T>
    where T: Copy {
    return (0..length).map(|_| value).collect();
}

// Given a length N, and sum S
// Find all sequences (x1, x2, ... xN), where
//   for all i, xi >= 0
//   the sum x1 + x2 + ... + xN = S
// Similar: https://en.wikipedia.org/wiki/Subset_sum_problem
pub fn sum_partitions(length: usize, sum: u32) -> impl Iterator<Item=Vec<u32>> {
    SumPartitions {
        stack: vec![(Vec::new(), 0)],
        length,
        sum
    }
}

struct SumPartitions {
    stack: Vec<(Vec<u32>, u32)>,
    length: usize,
    sum: u32
}

impl Iterator for SumPartitions {
    type Item = Vec<u32>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (mut values, total): (Vec<u32>, u32) = self.stack.pop()?;
            if values.len() == self.length - 1 {
                // Infer the last value
                values.push(self.sum - total);
                return Some(values);
            } else {
                // Push all adjacent values
                for v in 0..=(self.sum - total) {
                    let mut next_values: Vec<u32> = values.to_vec();
                    next_values.push(v);
                    self.stack.push((next_values, total + v));
                }
            }
        }
    }
}

// Regex 'findall' functionality
// The fancy-regex crate does not currently implement .find_iter() or .captures_iter() sadly
pub trait RegexExtension {
    fn findall<'t>(&self, text: &'t str) -> Vec<&'t str>;
}

impl RegexExtension for Regex {
    fn findall<'t>(&self, text: &'t str) -> Vec<&'t str> {
        let mut start: usize = 0;
        let mut matches: Vec<&'t str> = Vec::new();
        while let Some(m) = self.captures_from_pos(text, start).unwrap() {
            matches.push(m.get(1).unwrap().as_str());
            start = m.get(0).unwrap().end();
        }
        matches
    }
}

// Convenience methods for regex matching
pub trait CapturesExtension<'t> {
    fn get_as<F: FromStr>(&self, index: usize) -> F
        where <F as std::str::FromStr>::Err: std::fmt::Debug;

    fn get_str(&self, index: usize) -> &'t str;
}

impl<'t> CapturesExtension<'t> for Captures<'t> {
    fn get_as<F: FromStr>(&self, index: usize) -> F
        where <F as std::str::FromStr>::Err: std::fmt::Debug {
        self.get(index).unwrap().as_str().parse::<F>().unwrap()
    }

    fn get_str(&self, index: usize) -> &'t str {
        self.get(index).unwrap().as_str()
    }
}