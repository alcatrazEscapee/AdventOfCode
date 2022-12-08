#include "aoc.h"

size_t count(slice_t slice, size_t len, char c) {
    size_t total = 0;
    for (size_t i = 0; i < len; i++) {
        if (slice[i] == c) total++;
    }
    return total;
}

int main(args) {

    size_t len; slice_t inp = read_in(inp, len);
    slice_t next = new(slice_t)(len);

    size_t total = 0;
    for (size_t i = 0; i < 400000; i++) {
        if (i == 40) printf("Part 1: %d\n", total);

        total += count(inp, len, '.');

        for (size_t j = 0; j < len; j++) {
            #define trap(x) ((x) == '^')
            char left = j == 0 ? '.' : inp[j - 1], right = j == len - 1 ? '.' : inp[j + 1];
            next[j] = trap(left) != trap(right) ? '^' : '.';
        }

        slice_t temp = inp;
        inp = next;
        next = temp; // re-uses previous buffers for speed
    }

    printf("Part 2: %d\n", total);

    del(inp);
    del(next);

    return 0;
}