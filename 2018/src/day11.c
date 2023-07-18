#include "aoc.h"

#define SIZE 301

// Uses the summed area table to compute the max value of a given square
void compute_max_area(int* table, int width, int* maxWidth, int* maxX, int* maxY, int* maxArea);
int compute_area(int* table, int x, int y, int width);

// Builds the summed area table, using the given serial number (input) and `compute_value()`
int* compute_table(int serial);
int compute_value(int x, int y, int serial);

main {
    int serial;
    assert(scanf("%d\n", &serial) == 1, "reading input");

    // Uses a summed area table for efficient computation in part 2
    int* table = compute_table(serial);

    // Part 1, loop over and find the best 3x3 area
    int x = 0, y = 0, area = INT_MIN, width = 3;

    compute_max_area(table, 3, &width, &x, &y, &area);
    println("Part 1: %d,%d", x, y);

    // Part 2, compute everything from size 1 to 300
    for (int size = 1; size <= 300; size++) {
        if (size == 3) continue; // Micro optimization as this is already needlessly fast
        compute_max_area(table, size, &width, &x, &y, &area);
    }

    println("Part 2: %d,%d,%d", x, y, width);
}


void compute_max_area(int* table, int width, int* maxWidth, int* maxX, int* maxY, int* maxArea) {
    for (int x = 1; x <= SIZE - width; x++) {
        for (int y = 1; y <= SIZE - width; y++) {
            int area = compute_area(table, x, y, width);
            if (area > *maxArea) {
                *maxArea = area;
                *maxX = x;
                *maxY = y;
                *maxWidth = width;
            }
        }
    }
}

int compute_area(int* table, int x, int y, int width) {
    return table[(x - 1) + SIZE * (y - 1)]
        + table[(x - 1 + width) + SIZE * (y - 1 + width)]
        - table[(x - 1 + width) + SIZE * (y - 1)]
        - table[(x - 1) + SIZE * (y - 1 + width)];
}


int* compute_table(int serial) {
    int* table = (int*) malloc(sizeof(int) * SIZE * SIZE);
    
    memset(table, 0, SIZE * SIZE); // Fill in the base rows with zeros
    for (int x = 1; x < SIZE; x++) { // Fill in the rest of the table
        for (int y = 1; y < SIZE; y++) {
            table[x + SIZE * y]
                = table[(x - 1) + SIZE * y]
                + table[x + SIZE * (y - 1)]
                - table[(x - 1) + SIZE * (y - 1)]
                + compute_value(x, y, serial);
        }
    }
    return table;
}

int compute_value(int x, int y, int serial) {
    int rack_id = x + 10;
    int power_level = rack_id * y;
    power_level += serial;
    power_level *= rack_id;
    power_level = (power_level / 100) % 10;
    power_level -= 5;
    return power_level;
}