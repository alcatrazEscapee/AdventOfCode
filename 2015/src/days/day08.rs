
const INPUT: &str = include_str!("../../inputs/day08.txt");

pub fn part1() -> usize {
    INPUT.lines()
        .map(|line| {
            match decode_string(line) {
                Ok(s) => line.chars().count() - s.chars().count(),
                Err(err) => {
                    println!("Error parsing {}: {}", line, err);
                    0
                }
            }
        })
        .sum::<usize>()
}

pub fn part2() -> usize {
    INPUT.lines()
        .map(|line| encode_string(line).chars().count() - line.chars().count())
        .sum::<usize>()
}

/* S/SL Specification for the part 1 decoding
String:
    DoubleQuote
    {[
        | DoubleQuote:
            >
        | BackSlash:
            [
                | BackSlash, DoubleQuote:
                | LetterX:
                    Digit
                    Digit
            ]
        | *:
            ?
    ]};
 */

fn decode_string(s: &str) -> Result<String, String> {
    let mut chars = s.chars();
    let mut result: String = String::new();

    chars.next().ok_or(String::from("Empty string"))?.equal_or('\"', String::from("No open quote"))?;
    loop {
        match chars.next().ok_or(String::from("Unexpected end of input"))? {
            '\"' => break,
            '\\' => {
                match chars.next().ok_or(String::from("Expected escape sequence after \'\\\'"))? {
                    '\\' => result.push('\\'),
                    '\"' => result.push('\"'),
                    'x' => {
                        let mut hex = String::new();
                        for _ in 1..=2 {
                            hex.push(chars.next().ok_or("Unexpected end of input at \'\\x escape sequence")?.match_or(|c| is_hex_digit(&c), String::from("Expected hex digit as part of \'\\x escape sequence"))?);
                        }
                        result.push(u8::from_str_radix(hex.as_str(), 16).map_err(|e| e.to_string())? as char);
                    },
                    c => return Err(format!("Invalid escape sequence beginning with {}", c))
                }
            },
            c => result.push(c)
        };
    }
    Ok(result)
}

fn encode_string(s: &str) -> String {
    let mut result: String = String::new();
    result.push('\"');
    for c in s.chars() {
        match c {
            '\\' | '\"' => result.push('\\'),
            _ => {}
        };
        result.push(c);
    }
    result.push('\"');
    result
}

fn is_hex_digit(c: &char) -> bool {
    match *c {
        '0'..='9' | 'a'..='f' => true,
        _ => false
    }
}

trait Matchable where Self: std::marker::Sized {
    fn equal_or(&self, c: Self, err: String) -> Result<Self, String>;
    fn match_or(&self, condition: impl Fn(Self) ->bool, err: String) -> Result<Self, String>;
}

impl Matchable for char {

    fn equal_or(&self, c: char, err: String) -> Result<char, String> {
        match *self == c {
            true => Ok(*self),
            _ => Err(err)
        }
    }

    fn match_or(&self, condition: impl Fn(Self) -> bool, err: String) -> Result<Self, String> {
        match condition(*self) {
            true => Ok(*self),
            _ => Err(err)
        }
    }
}