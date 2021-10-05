use fancy_regex::Regex;
use std::collections::{HashMap, VecDeque};
use crate::utils::CapturesExtension;

const INPUT: &str = include_str!("../../inputs/day07.txt");

pub fn both() -> (u64, u64) {
    let mut all_rules: Vec<Rule> = parse();

    let part1: u64 = solve(&all_rules);

    all_rules.retain(|rule| {
        match rule {
            Rule::Unary(_, _, out) => *out != "b",
            _ => true
        }
    });

    all_rules.push(Rule::Unary(Value::Literal(part1), UnaryOperator::Buffer, "b"));

    let part2: u64 = solve(&all_rules);

    (part1, part2)
}

fn solve<'a>(rules_in: &Vec<Rule<'a>>) -> u64 {
    let mut rules: VecDeque<&Rule<'a>> = VecDeque::new();
    for rule in rules_in {
        rules.push_back(rule);
    }
    let mut signals: HashMap<&str, u64> = HashMap::new();
    loop {
        match rules.pop_front() {
            Some(rule) => {
                if !apply_rule(&mut signals, &rule) {
                    rules.push_back(rule);
                }
            },
            _ => break
        };
    }
    *signals.get("a").expect("Expected signal 'a' to be resolved")
}


enum UnaryOperator {
    Buffer,
    Not
}

enum BinaryOperator {
    And,
    Or,
    LShift,
    RShift
}

enum Value<'a> {
    Literal(u64),
    Reference(&'a str)
}

enum Rule<'a> {
    Unary(Value<'a>, UnaryOperator, &'a str),
    Binary(Value<'a>, Value<'a>, BinaryOperator, &'a str)
}

// Returns if the rule was successfully executed
fn apply_rule<'a>(signals: &mut HashMap<&'a str, u64>, rule: &Rule<'a>) -> bool {
    match rule {
        Rule::Unary(input_value, op, out) => {
            match apply_value(signals, input_value) {
                Some(inp) => signals.insert(out, apply_unary(inp, op)),
                None => return false
            };
        },
        Rule::Binary(lhs_value, rhs_value, op, out) => {
            match apply_value(signals, lhs_value) {
                Some(lhs) => match apply_value(signals, rhs_value) {
                    Some(rhs) => signals.insert(out, apply_binary(lhs, rhs, op)),
                    None => return false
                },
                None => return false
            };
        }
    }
    true
}

fn apply_value(signals: &mut HashMap<&str, u64>, value: &Value) -> Option<u64> {
    match value {
        Value::Literal(literal) => Some(*literal),
        Value::Reference(reference) => signals.get(reference).map(|x| *x)
    }
}

fn apply_unary(inp: u64, op: &UnaryOperator) -> u64 {
    match op {
        UnaryOperator::Buffer => inp,
        UnaryOperator::Not => !inp
    }
}

fn apply_binary(lhs: u64, rhs: u64, op: &BinaryOperator) -> u64 {
    match op {
        BinaryOperator::Or => lhs | rhs,
        BinaryOperator::And => lhs & rhs,
        BinaryOperator::LShift => lhs << rhs,
        BinaryOperator::RShift => lhs >> rhs
    }
}

fn parse<'a>() -> Vec<Rule<'a>> {
    let unary_re: Regex = Regex::new(r"^(NOT |)(\w+) -> (\w+)$").unwrap();
    let binary_re: Regex = Regex::new(r"^(\w+) (AND|OR|[LR]SHIFT) (\w+) -> (\w+)$").unwrap();
    INPUT.lines()
        .map(|line| {
            match unary_re.captures(line).unwrap() {
                Some(cs) => return Rule::Unary(parse_value(cs.get_str(2)), parse_unary(cs.get_str(1)), cs.get_str(3)),
                _ => {}
            };
            match binary_re.captures(line).unwrap() {
                Some(cs) => return Rule::Binary(parse_value(cs.get_str(1)), parse_value(cs.get_str(3)), parse_binary(cs.get_str(2)), cs.get_str(4)),
                _ => {}
            }
            panic!("Unable to parse line {}", line)
        })
        .collect()
}

// Parse a unary operator from it's textual name (or empty)
fn parse_unary(text: &str) -> UnaryOperator {
    match text {
        "NOT " => UnaryOperator::Not,
        _ => UnaryOperator::Buffer
    }
}

// Parse a binary operator from it's textual name
fn parse_binary(text: &str) -> BinaryOperator {
    match text {
        "AND" => BinaryOperator::And,
        "OR" => BinaryOperator::Or,
        "LSHIFT" => BinaryOperator::LShift,
        "RSHIFT" => BinaryOperator::RShift,
        _ => panic!("Unknown binary operator {}", text)
    }
}

// Parse either a literal integer value or a reference to a wire
fn parse_value(text: &str) -> Value {
    match text.parse::<u64>() {
        Ok(value) => Value::Literal(value),
        Err(_) => Value::Reference(text)
    }
}