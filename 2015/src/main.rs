
mod days;
mod utils;

fn main() {
    run_day(22);
}

fn run_all() {
    for day in 1u8..=25 {
        run_day(day)
    }
}

fn run_day(day: u8) {
    match days::run_day(day) {
        Ok(sln) => println!("Day {:2} | Part 1: {:15} | Part 2: {:15} | Time: {}", day, sln.part1, sln.part2, format_time(sln.nanos)),
        Err(err) => println!("Day {:2} | {}", day, err)
    }
}

fn format_time(nanos: u128) -> String {
    match nanos {
        0..=999 => format!("{} ns", nanos),
        1_000..=999_999 => format!("{} us", nanos / 1_000),
        1_000_000..=999_999_999 => format!("{} ms", nanos / 1_000_000),
        _ => format!("{} s", nanos / 1_000_000_000)
    }
}