use fancy_regex::Regex;
use crate::utils::{CapturesExtension, RegexExtension};
use crate::utils;
use std::collections::{HashMap, HashSet};

const RECIPES: &str = include_str!("../../inputs/day19.txt");
const MEDICINE: &str = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl";

pub fn both() -> (usize, usize) {
    let recipe_re: Regex = Regex::new(r"(e|[A-Z][a-z]?) => ((?:[A-Z][a-z]?)+)").unwrap();
    let atom_re: Regex = Regex::new(r"([A-Z][a-z]?)").unwrap();

    let mut atoms: Vec<&str> = Vec::new(); // index <-> atom name
    let mut recipes: HashMap<usize, Vec<Vec<usize>>> = HashMap::new();

    for line in RECIPES.lines() {
        let cs = recipe_re.captures(line).unwrap().unwrap();
        let inp: usize = utils::index_or_insert(&mut atoms, cs.get_str(1));
        let out: Vec<usize> = atom_re.findall(cs.get_str(2))
            .iter()
            .map(|a| utils::index_or_insert(&mut atoms, a))
            .collect();

        recipes.entry(inp).or_insert_with(|| Vec::new()).push(out); // poor man's computeIfAbsent(key, k -> new Vec()).push(v)
    }

    let medicine: Vec<usize> = atom_re.findall(MEDICINE)
        .iter()
        .map(|a| utils::index_or_insert(&mut atoms, a))
        .collect();

    let mut next_molecules: HashSet<Vec<usize>> = HashSet::new();
    for (i, atom) in medicine.iter().enumerate() {
        match recipes.get(atom) {
            Some(results) => {
                for result in results {
                    // Join medicine[...i] + result[...] + medicine[i...]
                    let mut joined: Vec<usize> = Vec::with_capacity(medicine.len() + result.len());
                    for j in 0..i {
                        joined.push(*medicine.get(j).unwrap());
                    }
                    for r in result {
                        joined.push(*r);
                    }
                    for j in i + 1..medicine.len() {
                        joined.push(*medicine.get(j).unwrap());
                    }
                    next_molecules.insert(joined);
                }
            }
            None => {},
        }
    }

    // Letting S := e, '(' := Rn, ')' := Ar, ',' := Y, the rules consist of the following productions:
    // S | X X
    // X | X X
    //   | X ( X )
    //   | X ( X , X )
    //   | X ( X , X , X )
    // All of the above productions have the same number of reductions in length, based on the count of '(', ')', and ',' in the original sequence.
    // So, we calculate this quantity = length - count('(', ')') - 2 * count(',') - 1

    let left_brace: usize = index_of(&atoms, "Rn");
    let right_brace: usize = index_of(&atoms, "Ar");
    let comma: usize = index_of(&atoms, "Y");

    (next_molecules.len(), medicine.len() - count(&medicine, left_brace) - count(&medicine, right_brace) - 2 * count(&medicine, comma) - 1)
}

fn index_of<T>(vec: &Vec<T>, e: T) -> usize
    where T : Eq {
    vec.iter().position(|p| *p == e).unwrap()
}

fn count(vec: &Vec<usize>, i: usize) -> usize {
    vec.iter().filter(|p| **p == i).count()
}