
const INPUT: &str = "3113322113";

pub fn both() -> (usize, usize) {
    let mut current: String = String::from(INPUT);
    for _ in 1..=40 {
        current = expand(&current);
    }
    let part1: usize = current.len();
    for _ in 1..=10 {
        current = expand(&current);
    }
    (part1, current.len())
}

fn expand(s: &String) -> String {
    let mut result: String = String::new();
    let mut chars = s.chars();
    let mut last: char = chars.next().unwrap();
    let mut count: usize = 1;

    #[inline]
    fn accumulate(s0: &mut String, c0: usize, c1: char) {
        s0.push_str(&c0.to_string());
        s0.push(c1);
    }

    for c in chars {
        if last != c {
            accumulate(&mut result, count, last);
            last = c;
            count = 0;
        }
        count += 1;
    }
    accumulate(&mut result, count, last);
    result
}