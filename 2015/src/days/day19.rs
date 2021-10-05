use fancy_regex::Regex;
use crate::utils::{CapturesExtension, RegexExtension};
use std::collections::{HashMap, HashSet};

const RECIPES: &str = include_str!("../../inputs/day19.txt");
const MEDICINE: &str = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl";

pub fn both() -> (usize, usize) {
    let recipe_re: Regex = Regex::new(r"(e|[A-Z][a-z]?) => ((?:[A-Z][a-z]?)+)").unwrap();
    let atom_re: Regex = Regex::new(r"([A-Z][a-z]?)").unwrap();

    let recipes: HashMap<&str, Vec<&str>> = RECIPES.lines()
        .map(|line| {
            let cs = recipe_re.captures(line).unwrap().unwrap();
            (cs.get_str(1), atom_re.findall(cs.get_str(2)).to_vec())
        })
        .collect();

    let next_molecules: HashSet<&str> = HashSet::new();
    let medicine_atoms: Vec<&str> = atom_re.findall(MEDICINE);
    for (i, atom) in medicine_atoms.iter().enumerate() {
        match recipes.get(atom) {
            Some(results) => {
                for result in results {

                }
            }
            None => {},
        }
    }

    (0, 0)
}