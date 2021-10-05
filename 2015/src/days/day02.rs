
const INPUT: &str = include_str!("../../inputs/day02.txt");

pub fn both() -> (u32, u32) {
    let mut part1: u32 = 0;
    let mut part2: u32 = 0;
    for dims in parse_input() {
        let sides: Vec<u32> = vec![dims[0] * dims[1], dims[1] * dims[2], dims[2] * dims[0]];
        let faces: Vec<u32> = vec![dims[0] + dims[1], dims[1] + dims[2], dims[2] + dims[0]];
        let smallest_side: u32 = *sides.iter().min().unwrap();
        part1 += sides.iter().sum::<u32>() * 2 + smallest_side;
        part2 += faces.iter().min().unwrap() * 2 + dims[0] * dims[1] * dims[2];
    }
    (part1, part2)
}

fn parse_input() -> impl Iterator<Item=Vec<u32>> {
    INPUT.lines()
        .map(|line| line.split("x").map(|d| d.parse::<u32>().unwrap()).collect())
}