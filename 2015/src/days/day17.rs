
const LENGTH: usize = 20;
const TARGET: u32 = 150;
const INPUT: [u32; LENGTH] = [11, 30, 47, 31, 32, 36, 3, 1, 5, 3, 32, 36, 15, 11, 46, 26, 28, 1, 19, 3];

pub fn both() -> (usize, usize) {
    let part1: usize = values().count();
    let min: u32 = values().map(|i| i.count_ones()).min().unwrap();
    let part2: usize = values().filter(|i| i.count_ones() == min).count();
    (part1, part2)
}

fn values() -> impl Iterator<Item=usize> {
    // Power set through bit manipulations!
    (0usize..(1 << LENGTH))
        .filter(|i|
            TARGET == (0usize..LENGTH)
                .map(|j| (((i >> j) & 1) as u32) * INPUT[j])
                .sum::<u32>()
        )
}