use std::collections::{BinaryHeap, HashSet};

struct Spell {
    name: &'static str,
    mana: i32,
    damage: i32,
    heal: i32,
    // Effect fields are turns of the relevant effect (values are constant) to apply
    effect_armor: i32,
    effect_damage: i32,
    effect_mana: i32
}

impl Spell {
    const fn new(name: &'static str, mana: i32, damage: i32, heal: i32, effect_armor: i32, effect_damage: i32, effect_mana: i32) -> Spell {
        Spell { name, mana, damage, heal, effect_armor, effect_damage, effect_mana }
    }
}

// Include a 'none' spell to have a explicit none turn, which means we can eliminate further states when you cannot cast a spell
const SPELLS: [Spell; 6] = [
    Spell::new("None", 0, 0, 0, 0, 0, 0),
    Spell::new("Magic Missile", 53, 4, 0, 0, 0, 0),
    Spell::new("Drain", 73, 2, 2, 0, 0, 0),
    Spell::new("Shield", 113, 0, 0, 6, 0, 0),
    Spell::new("Poison", 173, 0, 0, 0, 6, 0),
    Spell::new("Recharge", 229, 0, 0, 0, 0, 5)
];

const PLAYER_HP: i32 = 50;
const PLAYER_MANA: i32 = 500;

// Constants for each of the 'effects'
const EFFECT_ARMOR: i32 = 7;
const EFFECT_DAMAGE: i32 = 3;
const EFFECT_MANA: i32 = 101;

// Puzzle Input
const BOSS_HP: i32 = 59;
const BOSS_ATTACK: i32 = 9;

// The state of the game at any point in time. Equality doesn't include total mana spent, as we're interested only in the minimum mana per any given state
#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Debug, Hash)]
struct State {
    player_hp: i32,
    player_mana: i32,
    enemy_hp: i32,
    effect_armor: i32,
    effect_damage: i32,
    effect_mana: i32
}

impl State {
    fn new() -> State {
        State {
            player_hp: PLAYER_HP,
            player_mana: PLAYER_MANA,
            enemy_hp: BOSS_HP,
            effect_armor: 0,
            effect_damage: 0,
            effect_mana: 0
        }
    }

    fn start_of_turn_effects(&mut self) {
        // Apply start of turn effects, and decrement counters

        if self.effect_damage > 0 {
            self.effect_damage -= 1;
            self.enemy_hp -= EFFECT_DAMAGE;
        }
        if self.effect_mana > 0 {
            self.effect_mana -= 1;
            self.player_mana += EFFECT_MANA;
        }
        if self.effect_armor > 0 {
            self.effect_armor -= 1;
        }
    }

    fn can_cast(&self, spell: &Spell) -> bool {
        // Must have enough mana, and must not be casting a spell with an existing effect
        spell.mana <= self.player_mana && (spell.effect_armor == 0 || self.effect_armor == 0) && (spell.effect_damage == 0 || self.effect_damage == 0) && (spell.effect_mana == 0 || self.effect_mana == 0)
    }

    fn cast(&mut self, spell: &Spell) {
        // Cast a spell, applying all immediate and lasting effects
        self.player_hp += spell.heal;
        self.player_mana -= spell.mana;
        self.enemy_hp -= spell.damage;
        self.effect_armor += spell.effect_armor;
        self.effect_damage += spell.effect_damage;
        self.effect_mana += spell.effect_mana;
    }

    fn enemy_attack(&mut self) {
        // Perform the enemy attack sequence
        let armor: i32 = if self.effect_armor > 0 { EFFECT_ARMOR } else { 0 };
        self.player_hp -= std::cmp::max(1, BOSS_ATTACK - armor);
    }
}

pub fn both() -> (i32, i32) {
    (simulate_combat(false), simulate_combat(true))
}

fn simulate_combat(hard: bool) -> i32 {
    // Use a binary heap to always have the state of lowest mana usage at the top
    // We do a cheeky trick here: rather than using Reverse(), the 'mana' is just the negative of total mana spent
    let mut queue: BinaryHeap<(i32, State)> = BinaryHeap::new();

    // Important optimization: do not compute duplicate states
    // Without state tracking, this algorithm finishes with ~50 M states in the queue for part 1, and ~2M states for part 2. With this, we reach ~5k states for both parts.
    // The total runtime on my machine is ~1000x slower.
    let mut states: HashSet<State> = HashSet::new();

    let initial: State = State::new();
    queue.push((0, initial));
    states.insert(initial);

    loop {
        match queue.pop() {
            Some((mana, state)) => {
                if state.enemy_hp < 0 {
                    return -mana; // Exit condition. Negative because we cheat with our min/max heap trick.
                }
                if state.player_hp > 0 {
                    // Player is still alive, otherwise we just skip next turns
                    // Spells includes a 'wait' spell, and will return None if the spell failed to cast (reducing duplicates)
                    for spell in SPELLS {
                        match simulate_round(mana, &state, &spell, hard) {
                            Some((next_mana, next_state)) => if !states.contains(&next_state) {
                                queue.push((next_mana, next_state));
                                states.insert(next_state);
                            },
                            None => ()
                        }
                    }
                }
            },
            None => panic!("No win conditions?")
        }
    }
}

fn simulate_round(mana: i32, state: &State, spell: &Spell, hard: bool) -> Option<(i32, State)> {
    let mut s: State = state.clone();

    if hard {
        s.player_hp -= 1;
        if s.player_hp <= 0 {
            return None;
        }
    }

    s.start_of_turn_effects(); // Player
    if s.enemy_hp <= 0 {
        return Some((mana, s)) // Edge case: the boss dies at the start of the player turn, before the player casts a spell
    }

    if !s.can_cast(&spell) {
        return None;
    }

    s.cast(&spell);
    s.start_of_turn_effects(); // Boss
    s.enemy_attack();

    Some((mana - spell.mana, s))
}