
use crypto::digest::Digest;
use crypto::md5::Md5;
use std::sync::{Mutex, Arc};
use std::thread;

const INPUT: &str = "yzbqklnj";

struct Controller {
    part1: Option<u32>,
    part2: Option<u32>,
    counter: u32
}

impl Controller {
    fn new() -> Controller {
        Controller { part1: None, part2: None, counter: 0 }
    }
}

// This is vastly more complicated than it needs to be
// However, the simple single-threaded implementation was slow as bananas, which was unacceptable
// In addition, this presented an excellent opportunity to learn some of the concepts of concurrency in rust and engineer an effective solution
pub fn both() -> (u32, u32) {
    const THREADS: u32 = 8;
    const CHUNK: u32 = 1000;

    let increment: u32 = THREADS * CHUNK;
    let controller: Arc<Mutex<Controller>> = Arc::new(Mutex::new(Controller::new()));
    let mut handles= Vec::new();
    for _ in 0..THREADS {
        let controller: Arc<Mutex<Controller>> = Arc::clone(&controller);
        handles.push(thread::spawn(move || {
            let mut start: u32;
            let mut stop: u32;
            let mut md5: Md5 = Md5::new();
            let mut part1: Option<u32> = None;
            let mut part2: Option<u32> = None;
            loop {
                // Synchronized block
                {
                    let mut locked_controller = controller.lock().unwrap();

                    // Synchronize current and controller values
                    sync(&mut part1, &mut locked_controller.part1);
                    sync(&mut part2, &mut locked_controller.part2);

                    // If part 2 has been solved, terminate thread
                    if locked_controller.part2.is_some() {
                        break
                    }

                    // Prepare next chunk
                    start = locked_controller.counter;
                    locked_controller.counter += increment;
                    stop = locked_controller.counter;
                }

                // Async work, after locked controller is out of scope
                for offset in start..stop {
                    md5.input_str(&format!("{}{}", INPUT, offset));
                    let result = md5.result_str();
                    if !part1.is_some() && result.starts_with("00000") {
                        part1 = Some(offset);
                    } else if result.starts_with("000000") {
                        part1 = Some(offset);
                        part2 = Some(offset);
                        break
                    }
                    md5.reset();
                }
            }
        }));
    }
    // Join all threads
    for handle in handles {
        handle.join().unwrap();
    }

    let result_controller = controller.lock().unwrap();
    (result_controller.part1.expect("No part one solution"), result_controller.part2.expect("No part two solution"))
}

fn sync(left: &mut Option<u32>, right: &mut Option<u32>) {
    match left {
        Some(left_value) => match right {
            Some(right_value) => {
                *left = Some(std::cmp::min(*left_value, *right_value));
                *right = *left;
            },
            None => *right = *left
        },
        None => *left = *right
    };
}