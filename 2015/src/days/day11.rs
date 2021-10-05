const INPUT: &str = "cqjxjnds";
const LIO: [char; 3] = ['l', 'i', 'o'];

pub fn both() -> (String, String) {
    let mut password: Vec<char> = INPUT.chars().collect();
    let part1: String = find_next_password(&mut password);
    increment(&mut password);
    let part2: String = find_next_password(&mut password);
    (part1, part2)
}

fn find_next_password(mut password: &mut Vec<char>) -> String {
    while !is_valid_password(&password) {
        increment(&mut password);
    }
    return password.iter().collect();
}

// Returns Some() if the password was invalid. Returns None if valid
fn is_valid_password(password: &Vec<char>) -> bool {
    let mut inc_seq: usize = 1;
    let mut inc_seq_max: usize = 1;

    let mut pair_prev: Option<usize> = None;
    let mut pair_len: usize = 0;

    // Iter from Left -> Right
    let mut prev: Option<char> = None;
    for (i, c) in password.iter().enumerate() {
        if LIO.contains(c) {
            return false;
        }

        // Ascending sequences
        if prev.is_some() && ((prev.unwrap() as u8) + 1) as char == *c {
            inc_seq += 1;
            inc_seq_max = std::cmp::max(inc_seq, inc_seq_max);
        } else {
            inc_seq = 1;
        }

        // Non-overlapping pairs
        if prev.is_some() && prev.unwrap() == *c {
            if pair_prev.is_none() || pair_prev.unwrap() + 1 != i {
                pair_prev = Some(i);
                pair_len += 1;
            }
        }

        prev = Some(*c);
    }
    inc_seq_max >= 3 && pair_len >= 2
}

fn increment(password: &mut Vec<char>) {
    let mut index: usize = password.len() - 1;
    loop {
        if password[index] == 'z' {
            // Carry
            password[index] = 'a';
            if index == 0 {
                password.insert(0, 'a');
                break;
            }
            index -= 1;
        } else {
            password[index] = ((password[index] as u8) + 1) as char;
            break
        }
    }
}