use std::collections::HashMap;
use std::sync::{Mutex, MutexGuard, Arc};

use crypto::{md5::Md5, digest::Digest};
use fancy_regex::Regex;

mod aoc;

const THREADS: usize = 8;
const CHUNK_SIZE: usize = 100;

const LOOKAHEAD_SIZE: usize = 1000;
const TARGET_KEY: usize = 64;

fn main() {
    let input: String = aoc::read_input();
    let arc_part1: Arc<Mutex<Controller>> = aoc::run_async(THREADS, Controller::new(input.clone(), true), run_worker);
    println!("Part 1: {}", arc_part1.lock().unwrap().result_key());
    
    let arc_part2: Arc<Mutex<Controller>> = aoc::run_async(THREADS, Controller::new(input, false), run_worker);
    println!("Part 2: {}", arc_part2.lock().unwrap().result_key());
}

fn run_worker(controller: Arc<Mutex<Controller>>) {
    let mut md5: Md5 = Md5::new();

    let input: String = controller.lock().unwrap().input.clone();
    let part1: bool = controller.lock().unwrap().part1;
    let repeat_3: Regex = Regex::new(r"(.)\1\1").unwrap();
    let repeat_5: Regex = Regex::new(r"(.)\1\1\1\1").unwrap();

    let mut candidates: Vec<(usize, char)> = Vec::new();
    let mut verifiers: Vec<(usize, String)> = Vec::new();

    let mut start: usize = 0;
    let mut stop: usize = 0;

    loop {

        { // Sync work - transfer hashes to controller
            let mut lock: MutexGuard<Controller> = controller.lock().unwrap();

            // If the lock is already done, discard any work done
            if lock.is_done() {
                break
            }

            if stop != 0 {
                lock.chunks.insert(start, (candidates, verifiers));

                // See if we can process new hashes, if we have processed enough sequentially
                let mut next_to_process: usize = lock.max_processed;
                while let Some((c, v)) = lock.chunks.remove(&next_to_process) {
                    lock.process_chunk(c, v);
                    next_to_process += CHUNK_SIZE;
                }

                // If we just finished, then also don't send the worker out on another run
                if lock.is_done() {
                    break
                }

                candidates = Vec::new();
                verifiers = Vec::new();

                lock.max_processed = next_to_process;
            }

            // Acquire a new chunk to work on
            start = lock.max_dispatched;
            lock.max_dispatched += CHUNK_SIZE;
        }
        
        stop = start + CHUNK_SIZE;

        for index in start..stop {
            let hash: String = if part1 {
                hash_once(&mut md5, &input, index)
            } else {
                hash_2016(&mut md5, &input, index)
            };

            if let Some(c) = matches_and_capture(&repeat_3, &hash) {
                candidates.push((index, c));
            }
            if matches_and_capture(&repeat_5, &hash).is_some() {
                verifiers.push((index, hash));
            }
        }
    }
}

fn find_matching_quintuple(hash: &String, c: char) -> bool {
    hash.contains(String::from(c).repeat(5).as_str())
}

fn matches_and_capture(triple_regex: &Regex, hash: &String) -> Option<char> {
    triple_regex.captures(&hash)
        .unwrap()
        .map(|u| u.get(1)
            .unwrap()
            .as_str()
            .chars()
            .next()
            .unwrap())
}

fn hash_once(md5: &mut Md5, input: &String, offset: usize) -> String {
    md5.reset();
    md5.input_str(&format!("{}{}", input, offset));
    md5.result_str()
}

fn hash_2016(md5: &mut Md5, input: &String, offset: usize) -> String {
    let mut value: String = hash_once(md5, input, offset);
    for _ in 0..2016 {
        md5.reset();
        md5.input_str(value.as_str());
        value = md5.result_str();
    }
    value
}


struct Controller {
    candidates: HashMap<usize, char>,
    keys: Vec<usize>,

    input: String,
    part1: bool,

    max_dispatched: usize,
    max_processed: usize,

    chunks: HashMap<usize, (Vec<(usize, char)>, Vec<(usize, String)>)>,
}


impl Controller {
    fn new(input: String, part1: bool) -> Controller {
        Controller {
            candidates: HashMap::new(),
            keys: Vec::new(),
            input,
            part1,
            max_dispatched: 0,
            max_processed: 0,
            chunks: HashMap::new()
        }
    }

    fn process_chunk(self: &mut Self, candidates: Vec<(usize, char)>, verifiers: Vec<(usize, String)>) {
        for (i, c) in candidates {
            self.candidates.insert(i, c);
        }

        for (c_index, c) in &self.candidates {
            for (v_index, hash) in &verifiers {
                if c_index < v_index && *v_index < *c_index + LOOKAHEAD_SIZE && find_matching_quintuple(&hash, *c) {
                    if !self.keys.contains(c_index) {
                        self.keys.push(*c_index);
                    }
                }
            }
        }
    }

    fn is_done(self: &Self) -> bool { self.keys.len() > TARGET_KEY }

    fn result_key(self: &mut Self) -> usize { 
        self.keys.sort_unstable();
        *self.keys.get(TARGET_KEY - 1).unwrap() 
    }
}
