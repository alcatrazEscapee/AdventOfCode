#include "aoc.h"

#define SIZE 1295
#define WIDTH 1000

itype(unsigned char, 1) u8;

typedef struct {
    int id;
    int x, y;
    int w, h;
} claim_t;

main {
    claim_t* claims = (claim_t*) malloc(sizeof(claim_t) * SIZE);
    for (int i = 0; i < SIZE; i++) {
        claim_t* claim = &claims[i];
        int ret = scanf("#%d @ %d,%d: %dx%d\n", &claim->id, &claim->x, &claim->y, &claim->w, &claim->h);
        assert(ret == 5, "Error reading line %d, got %d", i, ret);
    }

    // Count the total number of points, which are within two or more claims
    // When in doubt? VERY LARGE BUFFER :D
    // Almost surprisingly, 1.6 MB of buffer and iterating claims, is actually faster (by an order of magnitude, 0.7s -> 0.03s)
    // than iterating every position x every claim, and checking membership.
    u8* overlaps = (u8*) malloc(SIZE * SIZE);
    
    memset(overlaps, 0, SIZE * SIZE);
    
    int count = 0;
    for (int i = 0; i < SIZE; i++) {
        claim_t* claim = &claims[i];

        for (int dx = 0; dx < claim->w; dx++) {
            for (int dy = 0; dy < claim->h; dy++) {
                int x = claim->x + dx, y = claim->y + dy;
                if (++overlaps[x + SIZE * y] == 2) {
                    count++;  // Increment as soon as we detect a single spot of overlap
                }
            }
        }
    }

    println("Part 1: %d", count);

    // Find the one claim that doesn't overlap any other claim
    // Intersections of square regions are fairly trivial to calculate, so we just loop over all disjoint pairs
    for (int i = 0; i < SIZE; i++) {
        
        bool overlap = false;
        for (int j = 0; j < SIZE; j++) {
            if (i == j) continue;

            if (!(
                claims[i].x >= claims[j].x + claims[j].w ||
                claims[i].x + claims[i].w <= claims[j].x ||
                claims[i].y >= claims[j].y + claims[j].h ||
                claims[i].y + claims[i].h <= claims[j].y
            )) {
                overlap = true;
                break;
            }
        }

        if (!overlap) {
            println("Part 2: %d", claims[i].id);
            return;
        }
    }
}