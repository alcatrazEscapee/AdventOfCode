use json::JsonValue;

const INPUT: &str = include_str!("../../inputs/day12.txt");

pub fn both() -> (i64, i64) {
    let input: JsonValue = json::parse(INPUT).unwrap();
    (sum(&input), sum_not_red(&input))
}

fn sum(j: &JsonValue) -> i64 {
    j.as_i64().unwrap_or(0) +
        j.entries()
            .map(|(_, v)| sum(v))
            .sum::<i64>() +
        j.members()
            .map(|v| sum(v))
            .sum::<i64>()
}

fn sum_not_red(j: &JsonValue) -> i64 {
    j.as_i64().unwrap_or(0) +
        j.members()
            .map(|v| sum_not_red(v))
            .sum::<i64>() +
        if j.entries()
            .all(|(_, v)| v.as_str()
                .map_or(true, |v| v != "red")) {
            j.entries()
                .map(|(_, v)| sum_not_red(v))
                .sum::<i64>()
        } else {
            0
        }
}

