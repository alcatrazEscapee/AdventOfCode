// This one was essentially solved by hand
// Structure of the assembly code:
// L1: jio a, +19
// This line skips between two different initializations of a
// The following instructions all just compute a value, either 9663 or 77671 for part 1 and 2 respectively
// The next section consists of the following (annotated) assembly code
// jio a, +8  | loop: if a == 1, return
// inc b      | b += 1
// jie a, +4  | if a != 1 {
// tpl a      |     a *= 3;
// inc a      |     a += 1;
// jmp +2     | } else {
// hlf a      |     a /= 2;
// jmp -7     | } goto loop;
// Which is an implementation to find the number of steps required to reach 1, using the setup of the Collatz Conjecture, for a given starting value of a

pub fn part1() -> u64 {
    collatz(9663)
}

pub fn part2() -> u64 {
    collatz(77671)
}

fn collatz(mut a: u64) -> u64 {
    let mut b: u64 = 0;
    while a != 1 {
        b += 1;
        if a % 2 == 1 {
            a = 3 * a + 1;
        } else {
            a /= 2;
        }
    }
    b
}