
const INPUT: &'static str = include_str!("../../inputs/day20.txt");

pub fn part1() -> usize {
    let input: usize = INPUT.parse::<usize>().unwrap();
    let size: usize = input / 10;
    let mut presents: Box<[usize]> = vec![0; size].into_boxed_slice();

    // Find lowest T s.t. the sum of the divisors of 10T >= INPUT
    for elf in 1..=size {
        for house in (elf..size).step_by(elf) {
            presents[house] += 10 * elf;
        }
    }
    first_element_greater_than(&presents, input)
}

pub fn part2() -> usize {
    let input: usize = INPUT.parse::<usize>().unwrap();
    let size: usize = input / 10;

    let mut presents: Box<[usize]> = vec![0; size].into_boxed_slice();
    for elf in 1..=size {
        for i in 1..=50 {
            let house = elf * i;
            if house < size {
                presents[elf * i] += 11 * elf;

            }
        }
    }
    first_element_greater_than(&presents, input)
}

fn first_element_greater_than(presents: &Box<[usize]>, e: usize) -> usize {
    for (i, present) in presents.iter().enumerate() {
        if *present >= e {
            return i
        }
    }
    panic!("No element found greater than {}", e)
}