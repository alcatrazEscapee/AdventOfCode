use std::time::Instant;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;
mod day22;
mod day23;
mod day24;
mod day25;

pub struct Solution {
    pub part1: String,
    pub part2: String,
    pub nanos: u128
}

impl Solution {
    fn new(part1: String, part2: String, nanos: u128) -> Solution {
        Self { part1, part2, nanos }
    }
}

pub fn run_day(day: u8) -> Result<Solution, String> {
    return match day {
        1 => run_both(|| day01::both()),
        2 => run_both(|| day02::both()),
        3 => run_parts(|| day03::part1(), || day03::part2()),
        4 => run_both(|| day04::both()),
        5 => run_parts(|| day05::part1(), || day05::part2()),
        6 => run_both(|| day06::both()),
        7 => run_both(|| day07::both()),
        8 => run_parts(|| day08::part1(), || day08::part2()),
        9 => run_both(|| day09::both()),
        10 => run_both(|| day10::both()),
        11 => run_both(|| day11::both()),
        12 => run_both(|| day12::both()),
        13 => run_both(|| day13::both()),
        14 => run_both(|| day14::both()),
        15 => run_both(|| day15::both()),
        16 => run_both(|| day16::both()),
        17 => run_both(|| day17::both()),
        18 => run_parts(|| day18::part1(), || day18::part2()),
        19 => run_both(|| day19::both()),
        20 => run_parts(|| day20::part1(), || day20::part2()),
        21 => run_both(|| day21::both()),
        22 => run_both(|| day22::both()),
        23 => run_parts(|| day23::part1(), || day23::part2()),
        24 => run_both(|| day24::both()),
        25 => run_parts(|| day25::part1(), || "n/a"),
        _ => Err(String::from("No solution"))
    };
}

fn run_parts<A: ToString, B: ToString>(part1: impl Fn() -> A, part2: impl Fn() -> B) -> Result<Solution, String> {
    run_both(|| (part1(), part2()))
}

fn run_both<A: ToString, B: ToString>(both_parts: impl Fn() -> (A, B)) -> Result<Solution, String> {
    let result: ((A, B), u128) = timeit(both_parts);
    let answers = result.0;
    Ok(Solution::new(answers.0.to_string(), answers.1.to_string(), result.1))
}

fn timeit<T>(part: impl Fn() -> T) -> (T, u128) {
    let now: Instant = Instant::now();
    let value: T = part();
    let time: u128 = now.elapsed().as_nanos();
    (value, time)
}

#[cfg(test)]
mod tests {

    #[test]
    fn day01() {
        test_day(1, "232", "1783");
    }

    #[test]
    fn day02() {
        test_day(2, "1588178", "3783758");
    }

    #[test]
    fn day03() {
        test_day(3, "2565", "2639");
    }

    #[test]
    fn day04() {
        test_day(4, "282749", "9962624");
    }

    #[test]
    fn day05() {
        test_day(5, "238", "69");
    }

    #[test]
    fn day06() {
        test_day(6, "400410", "15343601");
    }

    #[test]
    fn day07() {
        test_day(7, "16076", "2797");
    }

    #[test]
    fn day08() {
        test_day(8, "1371", "2117");
    }

    #[test]
    fn day09() {
        test_day(9, "117", "909");
    }

    #[test]
    fn day10() {
        test_day(10, "329356", "4666278");
    }

    #[test]
    fn day11() {
        test_day(11, "cqjxxyzz", "cqkaabcc");
    }

    #[test]
    fn day12() {
        test_day(12, "191164", "87842");
    }

    #[test]
    fn day13() {
        test_day(13, "618", "601");
    }

    #[test]
    fn day14() {
        test_day(14, "2660", "1256");
    }

    #[test]
    fn day15() {
        test_day(15, "222870", "117936");
    }

    #[test]
    fn day16() {
        test_day(16, "373", "260");
    }

    #[test]
    fn day17() {
        test_day(17, "4372", "4");
    }

    #[test]
    fn day18() {
        test_day(18, "768", "781");
    }

    #[test]
    fn day19() {
        test_day(19, "518", "200");
    }

    #[test]
    fn day20() {
        test_day(20, "786240", "831600");
    }

    #[test]
    fn day21() {
        test_day(21, "78", "148");
    }

    #[test]
    fn day22() {
        test_day(22, "1269", "1309");
    }

    #[test]
    fn day23() {
        test_day(23, "184", "231");
    }

    #[test]
    fn day24() {
        test_day(24, "11266889531", "77387711");
    }

    #[test]
    fn day25() {
        assert_eq!(crate::days::run_day(25).unwrap().part1, "9132360");
    }

    fn test_day(day: u8, first: &str, second: &str) {
        let sln = crate::days::run_day(day).unwrap();
        assert_eq!(sln.part1, first);
        assert_eq!(sln.part2, second);
    }
}