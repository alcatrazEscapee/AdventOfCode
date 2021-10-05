
struct Equipment {
    name: &'static str,
    cost: u32,
    damage: u32,
    armor: u32
}

impl Equipment {
    const fn new(name: &'static str, cost: u32, damage: u32, armor: u32) -> Equipment {
        Equipment { name, cost, damage, armor }
    }

    const fn none() -> Equipment {
        Equipment::new("None", 0, 0, 0)
    }
}

const WEAPONS: [Equipment; 5] = [
    Equipment::new("Dagger", 8, 4, 0),
    Equipment::new("Shortsword", 10, 5, 0),
    Equipment::new("Warhammer", 25, 6, 0),
    Equipment::new("Longsword", 40, 7, 0),
    Equipment::new("Greataxe", 74, 8, 0)
];

const ARMOR: [Equipment; 6] = [
    Equipment::none(),
    Equipment::new("Leather", 13, 0, 1),
    Equipment::new("Chainmail", 31, 0, 2),
    Equipment::new("Splintmail", 53, 0, 3),
    Equipment::new("Bandedmail", 75, 0, 4),
    Equipment::new("Platemail", 102, 0, 5)
];

const RINGS: [Equipment; 7] = [
    Equipment::none(),
    Equipment::new("Damage +1", 25, 1, 0),
    Equipment::new("Damage +2", 50, 2, 0),
    Equipment::new("Damage +3", 100, 3, 0),
    Equipment::new("Defense +1", 20, 0, 1),
    Equipment::new("Defense +2", 40, 0, 2),
    Equipment::new("Defense +3", 80, 0, 3)
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

// Puzzle Input
const BOSS: Character = Character::new(8, 1, 104);


pub fn both() -> (u32, u32) {
    let mut part1: Option<u32> = None; // Min cost victory
    let mut part2: Option<u32> = None; // Max cost defeat

    for i in 1..WEAPONS.len() * ARMOR.len() * RINGS.len() * RINGS.len() {
        let weapon: usize = (i) % WEAPONS.len();
        let armor: usize = (i / WEAPONS.len()) % ARMOR.len();
        let left_hand: usize = (i / (WEAPONS.len() * ARMOR.len())) % RINGS.len();
        let right_hand: usize = (i / (WEAPONS.len() * ARMOR.len() * RINGS.len())) % RINGS.len();

        if left_hand != right_hand || left_hand == EMPTY_HAND { // Cannot use the same ring (unless empty)
            let (player, cost): (Character, u32) = build(&ARMOR[armor], &WEAPONS[weapon], &RINGS[left_hand], &RINGS[right_hand]);

            // Don't run the fight unless we have to - if this fight would change either of the results
            if part1.is_none() || part2.is_none() || cost < part1.unwrap() || cost > part2.unwrap() {
                if fight(&player, &BOSS) {
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