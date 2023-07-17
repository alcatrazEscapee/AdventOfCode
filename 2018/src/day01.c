#include "aoc.h"

#define SIZE 1023
#define BYTES (1 << 16)
#define MAX (BYTES * 8)
#define HALF (MAX / 2)

#define BITSET_GET(bitset, value) (((bitset)[(value) >> 3] >> ((value) & 0b111)) & 0b1)
#define BITSET_SET(bitset, value) ((bitset)[(value) >> 3] |= 0b1 << ((value) & 0b111))

main {
    // Part 1 involves just finding the sum of all values
    // Parsing is fortunately quite easy this time, no thank you C
    // Store the values in a precisely sized array for the next part
    int* values = malloc(sizeof(int) * SIZE);
    int sum = 0;
    
    for (int i = 0; i < SIZE; i++) {
        assert(scanf("%d\n", &values[i]) == 1, "Reading input");
    
        sum += values[i];
    }

    println("Part 1: %d", sum);

    // Track unique values in a bitset
    // Since this can produce negative values, offset the value by half the bitset size
    byte* bitset = malloc(BYTES);
    int value = HALF;

    memset(bitset, 0, BYTES);

    for (int i = 0;;) {
        value += values[i];

        assert(0 <= value && value < MAX, "Out of range: %d", value);

        if (BITSET_GET(bitset, value)) {
            println("Part 2: %d", value - HALF);
            return;
        } else {
            BITSET_SET(bitset, value);
        }

        if (++i == SIZE) {
            i = 0;
        }
    }
}