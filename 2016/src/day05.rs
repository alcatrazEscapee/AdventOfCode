use std::collections::HashMap;
use std::sync::{Arc, Mutex, MutexGuard};

use crypto::digest::Digest;
use crypto::md5::Md5;

const PASSWORD_LENGTH: usize = 8;
const THREADS: usize = 8;
const CHUNK_SIZE: usize = 10000;

mod aoc;

fn main() {
    let input: String = aoc::read_input();
    let arc: Arc<Mutex<Controller>> = aoc::run_async(THREADS, Controller::new(input), run_worker);
    let controller: MutexGuard<Controller> = arc.lock().unwrap();

    println!("Part 1: {}\nPart 2: {}", controller.part1, controller.part2);
}

fn run_worker(controller: Arc<Mutex<Controller>>) {
    let mut start: usize = 0;
    let mut stop: usize = 0;

    let mut md5: Md5 = Md5::new();
    let mut hashes: Vec<(char, char)> = Vec::new();

    let input: String = controller.lock().unwrap().input.clone();
    
    loop {
        { // Sync work - transfer hashes to controller
            let mut lock: MutexGuard<Controller> = controller.lock().unwrap();

            // If the lock is already done, discard any work done
            if lock.is_done() {
                break
            }

            if stop != 0 {
                lock.hashes.insert(start, hashes);
                hashes = Vec::new();

                // See if we can process new hashes, if we have processed enough sequentially
                let mut next_to_process: usize = lock.max_processed;
                while let Some(v) = lock.hashes.remove(&next_to_process) {
                    lock.process_hashes(v);
                    next_to_process += CHUNK_SIZE;
                }

                // If we just finished, then also don't send the worker out on another run
                if lock.is_done() {
                    break
                }

                lock.max_processed = next_to_process;
            }

            // Acquire a new chunk to work on
            start = lock.max_dispatched;
            lock.max_dispatched += CHUNK_SIZE;
        }

        stop = start + CHUNK_SIZE;

        // Run the worker on that chunk, outside of synchronized block
        for index in start..stop {
            md5.input_str(&format!("{}{}", input, index));
            let result: String = md5.result_str();
            if result.starts_with("00000") {
                let char6: char = result.chars().nth(5).unwrap();
                let char7: char = result.chars().nth(6).unwrap();
    
                hashes.push((char6, char7));
            }
            md5.reset();
        }
    }
}


struct Controller {
    input: String,
    part1: String,
    part2: String,

    // The maximum offset that has been sent to a worker thread to compute hashes for
    max_dispatched: usize,
    // The maximum offset that has been processed, in sequential order
    max_processed: usize,

    // Computed hashes, as a map from start offset -> vector of hashes' (6th and 7th chars)
    hashes: HashMap<usize, Vec<(char, char)>>,

    // Progress counter for part 2
    progress: usize
}

impl Controller {
    fn new(input: String) -> Controller {
        Controller {
            input,
            part1: String::new(),
            part2: String::from("........"),
            max_dispatched: 0,
            max_processed: 0,
            hashes: HashMap::new(),
            progress: 0
        }
    }

    fn is_done(self: &Self) -> bool { self.progress >= PASSWORD_LENGTH }

    fn process_hashes(&mut self, hashes: Vec<(char, char)>) {
        for (char6, char7) in hashes {
            let mut found: bool = false;
            if self.part1.len() < 8 {
                self.part1.push(char6);
                found = true;
            }
    
            if let Some(d) = char6.to_digit(10).map(|x| x as usize) {
                if d < PASSWORD_LENGTH && self.part2.chars().nth(d).unwrap() == '.' {
                    self.part2.replace_range(d..d + 1, char7.to_string().as_str());
                    self.progress += 1;
                    found = true;
                }
            }
    
            if found {
                println!("Decrypting... {}{} -> {:.<8} / {}", char6, char7, self.part1, self.part2);
            }
    
            if self.progress == PASSWORD_LENGTH {
                return
            }
        }
    }
}
