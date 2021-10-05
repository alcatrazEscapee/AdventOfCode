
pub fn part1() -> u64 {
    let mut code: u64 = 20151125;
    for _ in 1..ord(2981, 3075) {
        code = (code * 252533) % 33554393;
    }
    code
}

fn ord(r: u32, c: u32) -> u32 {
    // Denote by ord(r, c) the ordinal at row r, column c
    // -> ord(1, c) is the Nth triangle number = c * (c + 1) / 2
    // -> ord(r, c) = ord(1, c + r - 1) - r + 1
    (c + r) * (c + r - 1) / 2 - r + 1
}