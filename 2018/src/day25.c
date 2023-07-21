#include "aoc.h"

#define SIZE 1498

typedef struct {
    int x, y, z, w;
} pos_t;

itype(unsigned short, 2) u16;

bool is_connected(pos_t* a, pos_t* b);


main {
    pos_t* points = (pos_t*) malloc(sizeof(pos_t) * SIZE);
    for (int i = 0; i < SIZE; i++) {
        pos_t* point = &points[i];

        assert(scanf("%d,%d,%d,%d\n", &point->x, &point->y, &point->z, &point->w) == 4, "reading input line %d", i);
    }

    // We need to find the number of connected components
    // In order to do this, we create a mapping of 'groups', which is just an parallel array to points
    // By adding a point to the groups one by one, we can compare if it is connected to any existing group, and use that group
    // If we find it belonging to multiple groups, we can merge subsequent points into the prior group
    u16* groups = (u16*) malloc(sizeof(u16) * SIZE);
    u16 next_group = 0; // The next available group number
    u16 unique_groups = 0;
    for (int i = 0; i < SIZE; i++) {
        pos_t* candidate = &points[i];
        int group = -1;

        // Iterate all prior points to look for connected groups
        for (int j = 0; j < i; j++) {
            pos_t* prior = &points[j];
            if (is_connected(prior, candidate)) {
                // If this candidate does not already belong to a group, assign it to the current one
                if (group == -1) {
                    group = groups[j];
                } else if (groups[j] != group) {
                    // Otherwise, we need to update all existing points that have this group, to point to this one
                    // This effectively removes a single group, all in one go
                    u16 to_remove = groups[j];
                    for (int k = 0; k < i; k++) {
                        if (groups[k] == to_remove) {
                            groups[k] = group;
                        }
                    }
                    unique_groups--;
                }
            }
        }

        if (group == -1) {
            // This point did not find an existing group, so mark it as belonging to a new one
            // Increment the total number of groups
            groups[i] = next_group;
            next_group++;
            unique_groups++;
        } else {
            groups[i] = group;
        }
    }

    println("Part 1: %d", unique_groups);
}

bool is_connected(pos_t* a, pos_t* b) {
    return abs(a->x - b->x) + abs(a->y - b->y) + abs(a->z - b->z) + abs(a->w - b->w) <= 3;
}