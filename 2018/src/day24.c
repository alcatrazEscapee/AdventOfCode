#include "aoc.h"

#define SIZE 10
#define RESISTS 3
#define DAMAGE_TYPES 10
#define DAMAGE_TYPE_NAME 20

#define HAS_RESIST(resists, type) (((resists)[0] == type) || ((resists)[1] == type) || ((resists)[2] == type))
#define UNIT_PTR(unit) (* (army_t**) (unit))


itype(unsigned char, 1) u8;

typedef u8 damage_t;

typedef struct army_s {
    struct army_s* target;
    int units;
    int hp;
    int dmg;
    u8 id;
    u8 init;
    damage_t weak[RESISTS];
    damage_t immune[RESISTS];
    damage_t type;
    bool is_target;
} army_t;


// Combat
int do_combat(army_t* immune, army_t* virus);

void do_target_selection(army_t** attacking_army, army_t** defending_army);
int sort_attacker_target_selection(const void* a, const void* b);
bool sort_defender_target_selection(const army_t* chosen, int chosen_dmg, const army_t* defender, int defender_dmg);

void do_attack_phase(army_t** all);
int sort_attack_phase(const void* a, const void* b);

// Methods operating on an entire army (army_t[SIZE])
int a_get_total_units(army_t** army);

// Methods operating on a single unit (army_t)
void u_do_attack_vs_enemy(const army_t* unit, army_t* enemy);
int u_get_potential_dmg_vs_enemy(const army_t* unit, const army_t* enemy);
int u_get_effective_power(const army_t* unit);
bool u_is_dead(const army_t* unit);

// Parsing
void scan_army(army_t army[SIZE], char** damage_types, int id_offset, const char* name);
damage_t scan_damage_type(char** damage_types, char* type);



main {
    army_t immune[SIZE], virus[SIZE];
    char* damage_types[DAMAGE_TYPES];

    memset(damage_types, 0, sizeof(char*) * DAMAGE_TYPES);

    assert(scanf("Immune System:\n") == 0, "reading first header");
    scan_army(immune, damage_types, 0, "immune system");
    assert(scanf("\nInfection:\n") == 0, "reading second header");
    scan_army(virus, damage_types, SIZE, "virus");

    println("Part 1: %d", abs(do_combat(immune, virus)));

    loop {
        // Apply a boost, +1 each loop iteration
        for (int i = 0; i < SIZE; i++) {
            immune[i].dmg++;
        }
        int result = do_combat(immune, virus);
        if (result > 0) {
            println("Part 2: %d", result); // Immune system won!
            break;
        }
    }
}



int do_combat(army_t* immune, army_t* virus) {

    // Local copies of both the immune, and virus units
    // We copy the raw bits of both into these arrays, then build pointers indexing them
    // No free required since these are stack allocated
    army_t immune_local[SIZE];
    army_t virus_local[SIZE];

    memcpy(immune_local, immune, sizeof(army_t) * SIZE);
    memcpy(virus_local, virus, sizeof(army_t) * SIZE);

    // Before starting combat, build mutable, sortable arrays of all units
    // These are arrays of pointers, and the pointers are stable pointers back to the original immune + virus arrays
    army_t* immune_ptr[SIZE];
    army_t* virus_ptr[SIZE];
    army_t* all_ptr[SIZE * 2];

    for (int i = 0; i < SIZE; i++) {
        all_ptr[i] = immune_ptr[i] = &immune_local[i];
        all_ptr[SIZE + i] = virus_ptr[i] = &virus_local[i];
    }

    // We can sometimes reach a stale situation, where neither side can damage each other
    // If we hit a few rounds where we notice this (no units lost) we assume we're stale, and return 0 instead
    int stale_counter = 0, total_units = -1;
    loop {

        do_target_selection(immune_ptr, virus_ptr);
        do_target_selection(virus_ptr, immune_ptr);
        
        do_attack_phase(all_ptr);

        int immune_total_units = a_get_total_units(immune_ptr),
             virus_total_units = a_get_total_units(virus_ptr);
        
        // Positive = immune won, Negative = virus won, Zero = stalemate
        if (virus_total_units == 0) return immune_total_units;
        if (immune_total_units == 0) return -virus_total_units;

        if (total_units == virus_total_units + immune_total_units) {
            stale_counter++;
            if (stale_counter == 5) {
                return 0; // Stale!
            }
        } else {
            stale_counter = 0;
            total_units = virus_total_units + immune_total_units;
        }
    }
}


void do_target_selection(army_t** attacking_army, army_t** defending_army) {
    // Targets are selected in order from effective power, then initiative
    // The target with the highest damage potential, then largest effective power, then initiative is selected
    // One attacker selects, and reserves, a single target
    //
    // On the attacking army, the `target` field is used to indicate the selected target
    // On the defending army, the `target` field is 0 if not targeted yet, or 1 if targeted

    // Reset chosen targets
    for (int i = 0; i < SIZE; i++) {
        attacking_army[i]->target = NULL;
        defending_army[i]->is_target = false;
    }

    // Sort based on which order to choose targets
    qsort(attacking_army, SIZE, sizeof(army_t*), sort_attacker_target_selection);

    // Then for each (alive) attacker, select iterate the defenders, and select the best target that is not already selected
    for (int i = 0; i < SIZE; i++) {
        army_t* attacker = attacking_army[i];
        army_t* chosen = NULL;
        int chosen_dmg = 0;
        
        if (u_is_dead(attacker)) continue;

        for (int j = 0; j < SIZE; j++) {
            army_t* defender = defending_army[j];

            if (u_is_dead(defender)) continue;
            if (defender->is_target) continue; // If this defender is already targeted, we can't double target

            int defender_dmg = u_get_potential_dmg_vs_enemy(attacker, defender);
            if (sort_defender_target_selection(chosen, chosen_dmg, defender, defender_dmg)) {
                chosen = defender;
                chosen_dmg = defender_dmg;
            }
        }

        if (chosen != NULL) {
            // If we chose a target, then record it in the `target` fields
            attacker->target = chosen; // We're targeting this unit
            chosen->is_target = true; // This unit has been targeted
        }
    }
}

int sort_attacker_target_selection(const void* a, const void* b) {
    const army_t* ua = UNIT_PTR(a);
    const army_t* ub = UNIT_PTR(b);
    const int power_a = u_get_effective_power(ua);
    const int power_b = u_get_effective_power(ub);

    return power_a != power_b ? power_b - power_a : ub->init - ua->init;
}

bool sort_defender_target_selection(const army_t* chosen, int chosen_dmg, const army_t* defender, int defender_dmg) {    
    if (defender_dmg == 0) return false; // Don't select as a target if we can't do damage
    if (chosen == NULL) return true; // If we can do damage, and we haven't selected any target yet, this will do

    if (defender_dmg != chosen_dmg) return defender_dmg > chosen_dmg; // Sort by damage

    int def_eff = u_get_effective_power(defender),
        chose_eff = u_get_effective_power(chosen);
    
    if (def_eff != chose_eff) return def_eff > chose_eff; // If it has more effective power
    
    return defender->init > chosen->init; // Finally, choose based on initiative
}


void do_attack_phase(army_t** all) {
    qsort(all, SIZE * 2, sizeof(army_t*), sort_attack_phase); // Sort all units, combined, by initiative

    for (int i = 0; i < SIZE * 2; i++) {
        army_t* unit = all[i];

        if (u_is_dead(unit)) continue; // Dead, cannot attack

        if (unit->target != NULL) { // If this unit has selected a target to attack
            u_do_attack_vs_enemy(unit, unit->target); // Then attack!
        }
    }
}

int sort_attack_phase(const void* a, const void* b) {
    const army_t* ua = UNIT_PTR(a);
    const army_t* ub = UNIT_PTR(b);
    return ub->init - ua->init;
}



int a_get_total_units(army_t** army) {
    int units = 0;
    for (int i = 0; i < SIZE; i++) {
        units += army[i]->units;
    }
    return units;
}



void u_do_attack_vs_enemy(const army_t* unit, army_t* enemy) {
    int dmg = u_get_potential_dmg_vs_enemy(unit, enemy);

    enemy->units -= dmg / enemy->hp;
    if (enemy->units < 0) {
        enemy->units = 0;
    }
}

int u_get_potential_dmg_vs_enemy(const army_t* unit, const army_t* enemy) {
    if (HAS_RESIST(enemy->immune, unit->type)) {
        return 0; // If the enemy is immune, it would take zero damage
    }
    if (HAS_RESIST(enemy->weak, unit->type)) {
        return u_get_effective_power(unit) * 2; // If the enemy is weak, it takes double damage
    }
    return u_get_effective_power(unit); // Otherwise, it takes 1.0x damage, equal to the effective power
}

int u_get_effective_power(const army_t* unit) {
    return unit->units * unit->dmg;
}

bool u_is_dead(const army_t* unit) {
    return unit->units <= 0;
}



/// Parses the input
/// This is the messiest parsing I have ever undertook in C (not like it would be much better in C++ tho...)
/// scanf my hero
void scan_army(army_t army[SIZE], char** damage_types, int id_offset, const char* name) {

#define INFO(x, ...) "reading army %s, line %d: " x, name, i , ## __VA_ARGS__
    
    char buffer[DAMAGE_TYPE_NAME];
    char c;
    for (int i = 0; i < SIZE; i++) {
        army_t* unit = &army[i];

        // Initialize
        memset(unit, 0, sizeof(army_t));
        for (int j = 0; j < RESISTS; j++) {
            unit->weak[j] = -1;
            unit->immune[j] = -1;
        }
        unit->id = id_offset + i;

        assert(scanf("%d units each with %d hit points ", &unit->units, &unit->hp) == 2, INFO("units + hp"));
        if (fgetc(stdin) == '(') { // Immune / Weakness
            fgetc(stdin); // i | w | w
            loop {
                c = fgetc(stdin); // m | e | i
                if (c == 'm' || c == 'e') { // Resist (either immune / weak), they can occur in either order because its stupid
                    assert(scanf(c == 'e' ? "ak to " : "mune to ") == 0, INFO("resist prefix"));
                    
                    damage_t* resists = c == 'm' ? unit->immune : unit->weak;
                    for (int j = 0;; j++) {
                        assert(scanf("%[^,;)]%c ", buffer, &c) == 2, INFO("resist entry %d", j));
                        
                        resists[j] = scan_damage_type(damage_types, buffer);

                        if (c != ',') {
                            fgetc(stdin);
                            break;
                        }
                    }
                } else {
                    assert(c == 'i', INFO("after reading resists, got %c", i));
                    break;
                }
            }
        } else {
            assert(fgetc(stdin) == 'i', INFO("skip no resists"));
        }

        assert(scanf("th an attack that does %d %s damage at initiative %hhd\n", &unit->dmg, buffer, &unit->init) == 3, INFO("dmg + init"));
        unit->type = scan_damage_type(damage_types, buffer);
    }
#undef INFO
}

damage_t scan_damage_type(char** damage_types, char* type) {
    for (int i = 0; i < DAMAGE_TYPES; i++) {
        if (damage_types[i] == NULL) { // Record a new type and return the index
            int len = strlen(type) + 1;
            damage_types[i] = (char*) malloc(len);
            memcpy(damage_types[i], type, len);
            return i;
        }
        if (strcmp(damage_types[i], type) == 0) { // Existing damage type found
            return i;
        }
    }
    unreachable;
}