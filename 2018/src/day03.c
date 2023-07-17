#include "aoc.h"

#define SIZE 1295
#define WIDTH 1000

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
    // The easiest way to do this without a large bitset or array is to just iterate each point
    int count = 0;
    for (int x = 0; x < WIDTH; x++) {
        for (int y = 0; y < WIDTH; y++) {
            int n = 0;
            for (int i = 0; i < SIZE; i++) {
                if (claims[i].x <= x &&
                    claims[i].x + claims[i].w > x &&
                    claims[i].y <= y &&
                    claims[i].y + claims[i].h > y
                ) {
                    n++;
                }
                
                if (n >= 2) {
                    count++;
                    break;
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