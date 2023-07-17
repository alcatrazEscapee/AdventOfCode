#include "aoc.h"

#define SIZE 50000
#define TO_UPPER(a) (a - 'a' + 'A')

int reduce(char* text, char skip);
bool matches(char a, char b);

main {
    char* text = malloc(SIZE);
    char* buffer = malloc(SIZE); // Re-use the same buffer for reductions for speed
    
    scanf("%s\n", text);
    memcpy(buffer, text, SIZE);
    
    println("Part 1: %d", reduce(buffer, '\0'));

    int min = INT_MAX;
    for (int c = 'A'; c <= 'Z'; c++) {
        memcpy(buffer, text, SIZE);
        int ret = reduce(buffer, c);
        if (ret < min) min = ret;
    }

    println("Part 2: %d", min);
}

// Performs in-place reduction of a polymer string
// The stack used is just the in-place result string, and the queue is the tail of the string
// Returns the total length of the reduced string
int reduce(char* text, char skip) {
    int stack = 0; // Index of the top of the stack
    char* next = text + 1; // Index of the next character to be pushed
    while (*next) {
        if (*next == skip || TO_UPPER(*next) == skip) {
            next++;
        } else if (stack != -1 && matches(text[stack], *next)) {
            stack--;
            next++;
        } else {
            stack++;
            text[stack] = *next;
            next++;
        }
    }
    return stack + 1;
}

bool matches(char a, char b) {
    return TO_UPPER(a) == b || a == TO_UPPER(b);
}