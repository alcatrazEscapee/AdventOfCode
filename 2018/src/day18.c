#include "aoc.h"

#include "../lib/set.h"

#define SIZE 50

typedef char** state_t;
typedef enum { Waiting, Cycle, After } phase_t;


state_t init();

void tick(state_t ptr, state_t next);
void swap(state_t* lhs, state_t* rhs);
void print(state_t state);

int get_score(state_t state);


main {
    state_t state = init(),
            next_state = init();

    // Read input offset by +1, +1
    for (int i = 0; i < SIZE; i++) {
        assert(scanf("%s\n", state[i + 1] + 1) == 1, "reading input line %d", i);
    }

    long minute = 0;
    for (; minute < 10; minute++) {
        tick(state, next_state);
        swap(&state, &next_state);
    }

    println("Part 1: %d", get_score(state));

    set_t* scores = set_new();

    // Part 2 eventually becomes cyclical, but this is difficult to detect
    // In theory, we could detect cycles of the actual grid - however, this is expensive in memory, and difficult to code
    // It is much easier to detect cycles in the score, but this is only a heuristic and can have false positives.
    // So, in order to avoid those, we look for a cycle in a few stages:
    //
    // 1. Wait until we see repeats, consecutively, for 'enough' time to prevent us seeing a false positive
    // 2. Pick an arbitrary time, and score, and count the duration until we see that again
    // 3. Jump ahead based on the identified cyclic duration, in terms of # of minutes
    // 4. Simulate normally the remaining minutes
    
    phase_t phase = Waiting;
    int repeats = 0, target = 0, score = 0;
    long target_minute = 0;

    #define END_MINUTE 1000000000L
    #define PROBABLY_A_CYCLE_AFTER_THIS_MANY_REPEATS 30

    for (; minute < END_MINUTE; minute++) {
        tick(state, next_state);
        swap(&state, &next_state);

        score = get_score(state);

        if (phase == Waiting) { // While waiting for a cycle to appear, insert into the set, and either reset or increment the repeats
            if (set_insert(scores, score)) {
                repeats++; // Found a repeat
            } else {
                repeats = 0;
            }

            if (repeats > PROBABLY_A_CYCLE_AFTER_THIS_MANY_REPEATS) { // Heuristic threshold to avoid false positives
                phase = Cycle;
                target = score;
                target_minute = minute;
            }
        } else if (phase == Cycle && score == target) { // In cycle phase, we wait until we see the previous score again
            long period_length = minute - target_minute;
            long periods = (END_MINUTE - minute) / period_length;
            
            minute += periods * period_length;
            phase = After;
        }
    }

    println("Part 2: %d", score);
}


state_t init() {
    state_t ptr = (state_t) malloc(sizeof(char*) * (SIZE + 2)); // +2 for buffers, so we can skip bounds checks
    for (int i = 0; i < SIZE + 2; i++) {
        ptr[i] = (char*) malloc(sizeof(char) * (SIZE + 2));
        memset(ptr[i], 0, SIZE + 2);
    }
    return ptr;
}


void tick(state_t state, state_t next_state) {
    for (int x = 1; x <= SIZE; x++) {
        for (int y = 1; y <= SIZE; y++) {
            switch (state[y][x]) {
                case '.':
                    int trees = 0;
                    for (int dx = -1; dx <= 1; dx++)
                        for (int dy = -1; dy <= 1; dy++)
                            if (state[y + dy][x + dx] == '|')
                                trees++;
                    next_state[y][x] = trees >= 3 ? '|' : '.';
                    break;
                case '|':
                    int yards = 0;
                    for (int dx = -1; dx <= 1; dx++)
                        for (int dy = -1; dy <= 1; dy++)
                            if (state[y + dy][x + dx] == '#')
                                yards++;
                    next_state[y][x] = yards >= 3 ? '#' : '|';
                    break;
                case '#':
                    bool yard = false, tree = false;
                    for (int dx = -1; dx <= 1; dx++)
                        for (int dy = -1; dy <= 1; dy++) {
                            if (state[y + dy][x + dx] == '|') tree = true;
                            if (state[y + dy][x + dx] == '#' && (dx != 0 || dy != 0)) yard = true;
                        }
                    next_state[y][x] = yard && tree ? '#' : '.';
                    break;
            }
        }
    }
}

void swap(state_t* lhs, state_t* rhs) {
    state_t t = *lhs;
    *lhs = *rhs;
    *rhs = t;
}

int get_score(state_t state) {
    int trees = 0, yards = 0;
    for (int x = 1; x <= SIZE; x++) {
        for (int y = 1; y <= SIZE; y++) {
            if (state[y][x] == '|') trees++;
            if (state[y][x] == '#') yards++;
        }
    }
    return trees * yards;
}

void print(state_t state) {
    for (int i = 0; i < SIZE; i++) {
        println("%s", state[i + 1] + 1);
    }
}