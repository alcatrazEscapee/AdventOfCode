#include "aoc.h"

#define SIZE 128
#define RULES 32

// We can index rules by a sequence of bits (1 or 0 for '#' or '.')
// HASH(c) computes this sequence, between [0, 32) for the leftmost pointer into a buffer
#define HASH(c) (HASH_I(c, 0) | HASH_I(c, 1) | HASH_I(c, 2) | HASH_I(c, 3) | HASH_I(c, 4))
#define HASH_I(c, i) ((c)[i] == '#' ? (1 << i) : 0)

int score(char* state, int len, int left);

main {
    char rules[RULES];
    char* state = (char*) malloc(sizeof(char) * SIZE); // Dynamically allocate because we free the state in a loop later

    memset(state, 0, SIZE);
    assert(scanf("initial state: %s\n\n", state + 4) == 1, "reading initial state");

    for (int i = 0; i < RULES; i++) {
        char buffer[5];
        char dest;
        assert(scanf("%5c => %c\n", buffer, &dest) == 2, "reading rule %d", i);
    
        rules[HASH(buffer)] = dest;
    }

    // Length of the 'core' segment
    // We have a 4-wide buffer on either size of the core segment, and memory is allocated with that in mind
    // This is so when creating the next state (which the core is +2 on either direction from previous),
    // We have enough room to read `state` at +4 and -4 from the core
    int len = SIZE - 8;
    // The offset of the 4th pot (the beginning of the 'core') from zero
    // This is used in computing the score.
    int left = 0;

    // The score on iteration 499, and 500, used for estimating part 2.
    int score499 = 0, score500 = 0;

    for (int i = 1; i <= 500; i++) {
        int next_len = len + 4;
        int next_left = left - 2;
        char* next_state = (char*) malloc(sizeof(char) * (next_len + 8));

        memset(next_state, 0, next_len + 8);

        for (int j = 0; j < next_len; j++) {
            char* offset = &state[j];
            next_state[j + 4] = rules[HASH(offset)];
        }

        free(state);

        len = next_len;
        left = next_left;
        state = next_state;

        if (i == 20) println("Part 1: %d", score(state, len, left));
        if (i == 499) score499 = score(state, len, left);
        if (i == 500) score500 = score(state, len, left);
    }

    // By inspection, the difference between successive iterations stabilizes after a certain point (here we iterate to 500)
    // Then we can simply compute the remaining difference
    println("Part 2: %lld", score500 + (score500 - score499) * (50000000000 - 500));
}


int score(char* state, int len, int left) {
    int sum = 0;
    for (int j = 0; j < len; j++) {
        if (state[j + 4] == '#') sum += left + j;
    }
    return sum;
}