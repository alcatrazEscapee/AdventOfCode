#include "aoc.h"

#define SIZE 50

#define MIN_MAX(min, max, value) do { \
    MIN(min, value); \
    MAX(max, value); \
} while (0)
#define MIN(min, value) if (value < min) min = value
#define MAX(max, value) if (value > max) max = value
#define L1(x0, y0, x1, y1) (abs((x0) - (x1)) + abs((y0) - (y1)))

typedef struct {
    int x, y;
    int size;
    bool infinite;
} point_t;


main {
    point_t* origins = (point_t*) malloc(sizeof(point_t) * SIZE);
    int minX = INT_MAX, minY = INT_MAX, maxX = INT_MIN, maxY = INT_MIN;
    for (int i = 0; i < SIZE; i++) {
        point_t* point = &origins[i];

        point->infinite = false;
        point->size = 0;

        assert(scanf("%d, %d\n", &point->x, &point->y) == 2, "Reading input");
    
        MIN_MAX(minX, maxX, point->x);
        MIN_MAX(minY, maxY, point->y);
    }

    // Infinite areas are detected by any area containing a point on the outer edge
    // Beyond that, all lines must be axis aligned and thus continue infinitely.
    int points10k = 0;
    for (int x = minX - 1; x <= maxX + 1; x++) {
        for (int y = minY - 1; y <= maxY + 1; y++) {
            
            int totalL1 = 0,
                minL1 = INT_MAX,
                minIndex = 0;
            
            for (int i = 0; i < SIZE; i++) {
                point_t* near = &origins[i];
                int l1 = L1(x, y, near->x, near->y);

                totalL1 += l1;
                if (l1 < minL1) {
                    minL1 = l1;
                    minIndex = i;
                }
            }

            point_t* point = &origins[minIndex];

            point->size++;
            point->infinite |= x < minX || x > maxX || y < minY || y > maxY;

            if (totalL1 < 10000) {
                points10k++;
            }
        }
    }

    int part1 = INT_MIN; // The size of the largest, non-infinite region
    for (int i = 0; i < SIZE; i++) {
        point_t* point = &origins[i];
        if (!point->infinite && point->size > part1) {
            part1 = point->size;
        }
    }

    println("Part 1: %d", part1);
    println("Part 2: %d", points10k);
}