#include "cordy.h"

#define MOD 2147483647
#define MASK16 ((1 << 16) - 1)
#define MUL_A 16807
#define MUL_B 48271


CORDY_EXTERN(do_part1) {
    ASSERT(n == 2);

    AS_INT(int64_t genA, args[0]);
    AS_INT(int64_t genB, args[1]);

    int64_t part1 = 0;
    for (int i = 0; i < 40000000; i++) {
        genA = (genA * MUL_A) % MOD;
        genB = (genB * MUL_B) % MOD;

        if ((genA & MASK16) == (genB & MASK16)) {
            part1 += 1;
        }
    }

    return INT(part1);
}

CORDY_EXTERN(do_part2) {
    ASSERT(n == 2);

    AS_INT(int64_t genA, args[0]);
    AS_INT(int64_t genB, args[1]);
    
    int64_t part2 = 0;
    for (int i = 0; i < 5000000; i++) {
        do {
            genA = (genA * MUL_A) % MOD;
        } while (genA & 3);

        do {
            genB = (genB * MUL_B) % MOD;
        } while (genB & 7);

        if ((genA & MASK16) == (genB & MASK16)) {
            part2 += 1;
        }
    }

    return INT(part2);
}