
const INPUT: &str = include_str!("../../inputs/day24.txt");

pub fn both() -> (i128, i128) {
    let mut weights: Vec<i32> = parse(INPUT);
    (solve(&weights, 3), solve(&weights, 4))
}

fn solve(weights: &Vec<i32>, partition: i32) -> i128 {
    let mut s: i128 = -1;
    solve_recursive(&weights, 0, weights.iter().sum::<i32>() / partition, 1, &mut s);
    s
}

fn solve_recursive(weights: &Vec<i32>, i: usize, space: i32, qe: i128, min_qe: &mut i128) {
    // Solves for the partition of weights s.t. the sum is equal to space and QE is minimized.
    // This has two major improvements over a flat iterative model over the power set of weights:
    // 1. It computes and re-uses the running space (difference between target and running sum), and running QE.
    //    This allows it to also shortcut subsets where there is no space, eliminating a large area of the problem space.
    //    The result of just these improvements reduced the runtime from 20 s -> ~200 ms (100x improvement)
    // 2. Instead of computing the minimum QE in a return value (as would be expected for a recursive function of this model), the minimum QE is stored in a globally readable mutable parameter.
    //    This allows us to perform another shortcut: exiting out of recursive branches when the current QE is greater than the already-encountered minimal QE.
    //    The result of this added an additional 10x improvement in runtime, reducing down to ~20 ms.
    // Note:
    // This function makes an assumption that the given state space is always partition-able, given there exists one subset with exactly 1 / partitions sum.
    // For this problem, this proved valid, and allowed us to eliminate a whole two tiers of power set checks.
    if space == 0 {
        if *min_qe == -1 || qe < *min_qe {
            *min_qe = qe;
        }
    } else if space > 0 && i < weights.len() && (*min_qe == -1 || qe < *min_qe) {
        let weight: i32 = weights[i];
        solve_recursive(&weights, i + 1, space, qe, min_qe);
        solve_recursive(&weights, i + 1, space - weight, qe * weight as i128, min_qe);
    }
}

fn min(a: Option<i128>, b: Option<i128>) -> Option<i128> {
    match a {
        Some(a0) => match b {
            Some(b0) => Some(std::cmp::min(a0, b0)),
            None => a
        }
        None => b
    }
}

fn parse(values: &str) -> Vec<i32> {
    values.lines()
        .map(|line| line.parse::<i32>().unwrap())
        .collect()
}