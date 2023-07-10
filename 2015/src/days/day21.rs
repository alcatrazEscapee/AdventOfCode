use fancy_regex::Regex;
use itertools::Itertools;

use crate::utils::RegexExtension;

const INPUT: &'static str = include_str!("../../inputs/day21.txt");

struct Equipment {
    cost: u32,
    damage: u32,
    armor: u32
}

impl Equipment {
    const fn new(cost: u32, damage: u32, armor: u32) -> Equipment {
        Equipment { cost, damage, armor }
    }

    const fn none() -> Equipment {
        Equipment::new(0, 0, 0)
    }
}

const WEAPONS: [Equipment; 5] = [
    Equipment::new(8, 4, 0), // Dagger
    Equipment::new(10, 5, 0), // Shortsword
    Equipment::new(25, 6, 0), // Warhammer
    Equipment::new(40, 7, 0), // Longsword
    Equipment::new(74, 8, 0) // Greataxe
];

const ARMOR: [Equipment; 6] = [
    Equipment::none(),
    Equipment::new(13, 0, 1), // Leather
    Equipment::new(31, 0, 2), // Chainmail
    Equipment::new(53, 0, 3), // Splintmail
    Equipment::new(75, 0, 4), // Bandedmail
    Equipment::new(102, 0, 5) // Platemail
];

const RINGS: [Equipment; 7] = [
    Equipment::none(),
    Equipment::new(25, 1, 0), // Damage +1
    Equipment::new(50, 2, 0), // Damage +2
    Equipment::new(100, 3, 0), // Damage +3
    Equipment::new(20, 0, 1), // Defense +1
    Equipment::new(40, 0, 2), // Defense +2
    Equipment::new(80, 0, 3) // Defense +3
];

const EMPTY_HAND: usize = 0;
const PLAYER_HP: i32 = 100;

// Character Templates

struct Character {
    damage: i32,
    armor: i32,
    hp: i32
}

impl Character {
    const fn new(damage: i32, armor: i32, hp: i32) -> Character {
        Character { damage, armor, hp }
    }
}


pub fn both() -> (u32, u32) {
    let mut part1: Option<u32> = None; // Min cost victory
    let mut part2: Option<u32> = None; // Max cost defeat

    let (boss_hp, boss_damage, boss_armor) = Regex::new(r"(\d+)").unwrap()
        .findall(INPUT)
        .into_iter()
        .map(|u| u.parse::<i32>().unwrap())
        .collect_tuple()
        .unwrap();
    let boss: Character = Character::new(boss_damage, boss_armor, boss_hp);

    for i in 1..WEAPONS.len() * ARMOR.len() * RINGS.len() * RINGS.len() {
        let weapon: usize = (i) % WEAPONS.len();
        let armor: usize = (i / WEAPONS.len()) % ARMOR.len();
        let left_hand: usize = (i / (WEAPONS.len() * ARMOR.len())) % RINGS.len();
        let right_hand: usize = (i / (WEAPONS.len() * ARMOR.len() * RINGS.len())) % RINGS.len();

        if left_hand != right_hand || left_hand == EMPTY_HAND { // Cannot use the same ring (unless empty)
            let (player, cost): (Character, u32) = build(&ARMOR[armor], &WEAPONS[weapon], &RINGS[left_hand], &RINGS[right_hand]);

            // Don't run the fight unless we have to - if this fight would change either of the results
            if part1.is_none() || part2.is_none() || cost < part1.unwrap() || cost > part2.unwrap() {
                if fight(&player, &boss) {
                    part1 = match part1 {
                        Some(value) => Some(std::cmp::min(value, cost)),
                        None => Some(cost)
                    }
                } else {
                    part2 = match part2 {
                        Some(value) => Some(std::cmp::max(value, cost)),
                        None => Some(cost)
                    }
                }
            }
        }
    }
    (part1.expect("No victory condition?"), part2.expect("No defeat possible?"))
}

fn build(armor: &Equipment, weapon: &Equipment, left_hand: &Equipment, right_hand: &Equipment) -> (Character, u32) {
    (Character::new(
        (armor.damage + weapon.damage + left_hand.damage + right_hand.damage) as i32,
        (armor.armor + weapon.armor + left_hand.armor + right_hand.armor) as i32,
        PLAYER_HP
    ), armor.cost + weapon.cost + left_hand.cost + right_hand.cost)
}

fn fight(player: &Character, enemy: &Character) -> bool {
    // Returns true if the player wins
    let mut player_hp: i32 = player.hp as i32;
    let mut enemy_hp: i32 = enemy.hp as i32;
    loop {
        // Player goes first
        enemy_hp -= std::cmp::max(1, player.damage - enemy.armor);
        if enemy_hp <= 0 {
            return true;
        }
        player_hp -= std::cmp::max(1, enemy.damage - player.armor);
        if player_hp <= 0 {
            return false;
        }
    }
}