// Structure of the input:
//
// jio a, +19   | This jumps between two different initializations of `a`
// ...
// jio a, +8  | loop: if a == 1, return
// inc b      | b += 1
// jie a, +4  | if a != 1 {
// tpl a      |     a *= 3;
// inc a      |     a += 1;
// jmp +2     | } else {
// hlf a      |     a /= 2;
// jmp -7     | } goto loop;
//
// Which we recognize as the Collatz Conjecture!
// However it's fairly trivial to write an interpreter
// for this specific language, and given the problem size, it performs well.
// And it was fun to write.


const INPUT: &'static str = include_str!("../../inputs/day23.txt");

#[derive(Debug)]
enum Opcode {
    IncA,
    IncB,
    Mul3A,
    Rhs1A,
    JmpIfA1(u8),
    JmpIfAEven(u8),
    Jmp(u8),
    Ret,
}


pub fn both() -> (u64, u64) {
    let code: Vec<Opcode> = parse();
    (run(&code, 0), run(&code, 1))
}

fn run(code: &Vec<Opcode>, mut a: u64) -> u64 {
    let mut b: u64 = 0;
    let mut ip: usize = 0;

    loop {
        match code[ip] {
            Opcode::IncA => a += 1,
            Opcode::IncB => b += 1,
            Opcode::Mul3A => a *= 3,
            Opcode::Rhs1A => a >>= 1,
            Opcode::JmpIfA1(jmp) => if a == 1 { ip = jmp as usize },
            Opcode::JmpIfAEven(jmp) => if (a & 1) == 0 { ip = jmp as usize },
            Opcode::Jmp(jmp) => ip = jmp as usize,
            Opcode::Ret => break,
        }
        ip += 1;
    }
    b
}

fn parse() -> Vec<Opcode> {
    let mut code: Vec<Opcode> = INPUT.lines()
        .enumerate()
        .map(|(ip, line)| match line {
            "inc a" => Opcode::IncA,
            "inc b" => Opcode::IncB,
            "tpl a" => Opcode::Mul3A,
            "hlf a" => Opcode::Rhs1A,
            _ if line.starts_with("jio a") => Opcode::JmpIfA1((ip as i8 + line[7..].parse::<i8>().unwrap() - 1) as u8),
            _ if line.starts_with("jie a") => Opcode::JmpIfAEven((ip as i8 + line[7..].parse::<i8>().unwrap() - 1) as u8),
            _ if line.starts_with("jmp") => Opcode::Jmp((ip as i8 + line[4..].parse::<i8>().unwrap() - 1) as u8),
            _ => panic!("Unsupported: {}", line)
        })
        .collect();
    code.push(Opcode::Ret);
    code
}
