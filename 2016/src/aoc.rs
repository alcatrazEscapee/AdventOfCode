// Allow dead code, since this module's helpers aren't all used by each individual compilation target (day)
#![allow(dead_code)]

use std::io::{self, BufRead};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::marker::Send;

pub fn read_input() -> String {
    io::stdin()
        .lock()
        .lines()
        .next()
        .unwrap()
        .unwrap()
}

pub fn run_async<'a, T, F>(threads: usize, controller: T, worker: F) -> Arc<Mutex<T>> where
    T: Send + 'static,
    F: Fn(Arc<Mutex<T>>) -> (),
    F: Send + 'static,
    F: Copy,
{
    let arc: Arc<Mutex<T>> = Arc::new(Mutex::new(controller));
    let mut handles: Vec<JoinHandle<()>> = Vec::new();
    for _ in 0..threads {
        let local: Arc<Mutex<T>> = Arc::clone(&arc);
        handles.push(thread::spawn(move || {
            worker(local);
        }))
    }

    for handle in handles {
        handle.join().unwrap();
    }

    arc
}