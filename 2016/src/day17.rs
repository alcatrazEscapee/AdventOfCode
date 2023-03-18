use crypto::{md5::Md5, digest::Digest};


const DIRECTIONS: [(i8, i8, char); 4] = [(0, -1, 'U'), (0, 1, 'D'), (-1, 0, 'L'), (1, 0, 'R')];

mod aoc;

fn main() {
    let input: String = aoc::read_input();
    let mut queue: Vec<(i8, i8, String)> = vec![(0, 0, String::new())];
    let mut md5: Md5 = Md5::new();
    
    let mut shortest_path: Option<String> = None;
    let mut longest_path: usize = 0;

    while let Some((x, y, path)) = queue.pop() {
        let result: String = hash(&mut md5, &input, &path);
        let mut chars = result.chars();
        for (dx, dy, dir) in DIRECTIONS {
            if is_open(chars.next().unwrap()) {
                let x0: i8 = x + dx;
                let y0: i8 = y + dy;
                if x0 >= 0 && y0 >= 0 && x0 < 4 && y0 < 4 {
                    let next_path: String = format!("{}{}", path, dir);
                    let next_path_len: usize = next_path.len();
                    if x0 == 3 && y0 == 3 {
                        shortest_path = match shortest_path {
                            None => Some(next_path),
                            Some(path) if path.len() > next_path_len => Some(next_path),
                            path => path,
                        };
                        longest_path = longest_path.max(next_path_len);
                    } else {
                        queue.push((x0, y0, next_path));
                    }
                }
            }
        }
    }

    println!("Part 1: {}", shortest_path.unwrap());
    println!("Part 2: {}", longest_path);
}

fn hash(md5: &mut Md5, input: &String, path: &String) -> String {
    md5.reset();
    md5.input_str(&format!("{}{}", input, path));
    md5.result_str()
}

fn is_open(c: char) -> bool {
    match c {
        'b' | 'c' | 'd' | 'e' | 'f' => true,
        _ => false,
    }
}