
const INPUT: &str = include_str!("../../inputs/day01.txt");

pub fn both() -> (i32, usize) {
    let mut floor: i32 = 0;
    let mut basement_index: Option<usize> = None;
    for c in INPUT.char_indices() {
        floor += match c.1 {
            '(' => 1,
            ')' => -1,
            _ => 0
        };
        if floor == -1 && basement_index.is_none() {
            basement_index = Some(1 + c.0);
        }
    }
    (floor, basement_index.expect("Never reached the basement!"))
}