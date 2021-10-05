
const INPUT: usize = 34000000;
const SIZE: usize = INPUT / 10;

pub fn part1() -> usize {
    // Find lowest T s.t. the sum of the divisors of 10T >= INPUT
    let mut presents: Box<[usize]> = vec![0; SIZE].into_boxed_slice();
    for elf in 1..=SIZE {
        for house in (elf..SIZE).step_by(elf) {
            presents[house] += 10 * elf;
        }
    }
    first_element_greater_than(&presents, INPUT)
}

pub fn part2() -> usize {
    let mut presents: Box<[usize]> = vec![0; SIZE].into_boxed_slice();
    for elf in 1..=SIZE {
        for i in 1..=50 {
            let house = elf * i;
            if house < SIZE {
                presents[elf * i] += 11 * elf;

            }
        }
    }
    first_element_greater_than(&presents, INPUT)
}

fn first_element_greater_than(presents: &Box<[usize]>, e: usize) -> usize {
    for (i, present) in presents.iter().enumerate() {
        if *present >= INPUT {
            return i
        }
    }
    panic!("No element found greater than {}", e)
}