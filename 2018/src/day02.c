#include "aoc.h"

#define SIZE 250
#define BUFFER 32

itype(unsigned char, 1) u8;


main {
    char** lines = (char**) malloc(sizeof(char*) * SIZE);
    for (int i = 0; i < SIZE; i++) {
        lines[i] = (char*) malloc(sizeof(char*) * BUFFER);
        memset(lines[i], 0, BUFFER);

        assert(scanf("%s\n", lines[i]) == 1, "Reading line %d", i);
    }

    // Part 1 requires counting unique occurrences of each character
    // For each line, we store the counts in a buffer indexed by character - 'a'
    int pairs = 0, triples = 0;
    u8 counts[BUFFER] = { 0 };
    
    for (int i = 0; i < SIZE; i++) {
        memset(counts, 0, BUFFER);
        
        for (char* ptr = lines[i]; *ptr != 0; ptr++) {
            counts[*ptr - 'a']++;
        }

        bool pair = false, triple = false;
        for (int j = 0; j < BUFFER; j++) {
            switch (counts[j]) {
                case 2:
                    pair = true;
                    break;
                case 3:
                    triple = true;
                    break;
            }
        }

        if (pair) pairs++;
        if (triple) triples++;
    }

    println("Part 1: %d", pairs * triples);

    // Part 2 involves finding a pair of IDs with exactly one character difference
    // Iterate over the set of IDs in a cross product, then scan for differences
    int i, j;
    for (i = 0; i < SIZE; i++) {
        for (j = 0; j < SIZE; j++) {
            if (i == j) continue;
            
            int diffs = 0;
            for (int p = 0; p < BUFFER && lines[i][p] != 0; p++) {
                if (lines[i][p] != lines[j][p]) {
                    diffs++;
                }
            }

            if (diffs == 1) goto end;
        }
    }
    end: ;
    
    // The answer is the letters common to the two strings
    // To do that, we need to create another buffer for the common characters
    char* answer = (char*) malloc(sizeof(char) * BUFFER);
    memset(answer, 0, BUFFER);

    for (int p = 0, q = 0; p < BUFFER && lines[i][p] != 0; p++) {
        if (lines[i][p] == lines[j][p]) {
            answer[q] = lines[i][p];
            q++;
        }
    }

    println("Part 2: %s", answer);
}