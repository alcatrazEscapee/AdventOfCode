#include "aoc.h"

#define SIZE 332

#define MIN_MAX(min, max, value) do { \
    MIN(min, value); \
    MAX(max, value); \
} while (0)
#define MIN(min, value) if (value < min) min = value
#define MAX(max, value) if (value > max) max = value

typedef struct {
    int x, y;
    int vx, vy;
} particle_t;

main {
    particle_t* particles = (particle_t*) malloc(sizeof(particle_t) * SIZE);
    for (int i = 0; i < SIZE; i++) {
        particle_t* p = &particles[i];

        assert(scanf("position=<%d, %d> velocity=<%d, %d>\n", &p->x, &p->y, &p->vx, &p->vy) == 4, "reading input line %d", i);
    }

    for (int t = 1;; t++) {
        int minY = INT_MAX, maxY = INT_MIN, minX = INT_MAX, maxX = INT_MIN;
        for (int i = 0; i < SIZE; i++) {
            particle_t* p = &particles[i];

            p->x += p->vx;
            p->y += p->vy;

            MIN_MAX(minX, maxX, p->x);
            MIN_MAX(minY, maxY, p->y);
        }

        if (maxY - minY <= 10) { // Heuristically detect the end when the particles all are within a certain y range

            // In order to print the letters, we assemble a buffer, then print it in lines
            // Width + 1 for the null terminator
            int width = 2 + maxX - minX, height = 1 + maxY - minY;
            char* buffer = (char*) malloc(sizeof(char) * width * height);

            // Initialize with '.' x width, followed by '\0'
            memset(buffer, '.', width * height);
            for (int y = 1; y <= height; y++) {
                buffer[width * y - 1] = '\0';
            }

            // We know it will be in-bounds due to assembling the right size of buffer
            for (int i = 0; i < SIZE; i++) {
                particle_t* p = &particles[i];
                buffer[(p->x - minX) + width * (p->y - minY)] = '#';
            }

            println("Part 1:");
            for (int y = 0; y < height; y++) {
                println("%s", &buffer[width * y]);
            }

            println("Part 2: %d", t);
            return;
        }
    }
}