const TARGET: u32 = 150;
const INPUT: &'static str = include_str!("../../inputs/day17.txt");

pub fn both() -> (usize, usize) {
    let input: Vec<u32> = INPUT.lines()
        .map(|u| u.parse::<u32>().unwrap())
        .collect();
    let part1: usize = values(&input).count();
    let min: u32 = values(&input).map(|i| i.count_ones()).min().unwrap();
    let part2: usize = values(&input).filter(|i| i.count_ones() == min).count();
    (part1, part2)
}

fn values(input: &Vec<u32>) -> impl Iterator<Item=usize> + '_ {
    // Power set through bit manipulations!
    let length: usize = input.len();
    (0usize..(1 << length))
        .filter(move |i|
            TARGET == (0usize..length)
                .map(|j| (((i >> j) & 1) as u32) * input[j])
                .sum::<u32>()
        )
}