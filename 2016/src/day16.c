#include "aoc.h"

slice_t dragon(slice_t inp, size_t len, size_t target);

main
{
    read_input(inp, len);

    slice_t part1 = dragon(inp, len, 272);
    slice_t part2 = dragon(inp, len, 35651584);

    printf("Part 1: %s\nPart 2: %s\n", part1, part2);

    del(part1);
    del(part2);
    del(inp);

    return 0;
}

slice_t dragon(slice_t inp, size_t len, size_t target) {
    slice_t prev = copy(slice_t)(inp);
    slice_t next = NULL;

    while (len < target) {
        next = new(slice_t)(2 * len + 1);
        memcpy(next, prev, len);
        next[len] = '0';
        for (size_t i = 0; i < len; i++) {
            next[len + i + 1] = prev[len - i - 1] == '0' ? '1' : '0';
        }
        del(prev);
        prev = next;
        len = 2 * len + 1;
    }

    len = target;

    while ((len & 1) == 0) {
        len = len >> 1;
        next = new(slice_t)(len);
        for (size_t i = 0; i < len; i++) {
            next[i] = prev[(i << 1)] == prev[(i << 1) | 1] ? '1' : '0';
        }
        del(prev);
        prev = next;
    }
    return next;
}