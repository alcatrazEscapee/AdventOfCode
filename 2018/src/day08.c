#include "aoc.h"

#define SIZE 16000
#define NEXT(buffer) *(*buffer)++

void parse(int** buffer, int* part1, int* part2);

main {
    int* buffer = (int*) malloc(sizeof(int) * SIZE);
    int part1 = 0, part2 = 0;
    for (int i = 0, ret = 1; ret == 1; i++) {
        ret = scanf("%d", &buffer[i]);
    }
    
    parse(&buffer, &part1, &part2);

    println("Part 1: %d", part1);
    println("Part 2: %d", part2);
}

void parse(int** buffer, int* part1, int* part2) {
    int children = NEXT(buffer);
    int metadata = NEXT(buffer);

    int* values = NULL;
    if (children > 0) {
        // Allocate memory for the storing the value of each child node
        values = (int*) malloc(sizeof(int) * children);
        for (int i = 0; i < children; i++) {
            parse(buffer, part1, &values[i]);
        }
    }

    *part2 = 0; // Part 2 is the value of this current node
    for (int i = 0; i < metadata; i++) {
        int entry = NEXT(buffer);
        
        if (children == 0) { // Nodes with no children take the value of the sum of their metadata
            *part2 += entry;
        } else if (entry > 0 && entry <= children) { // Entry refers to an existing child
            *part2 += values[entry - 1]; // We know `values` won't be null at this point since we must have children
        }

        *part1 += entry; // Part 1 is the raw sum of all metadata entries 
    }

    free(values); // free(NULL) is a no-op
}