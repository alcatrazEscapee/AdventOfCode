// Once again, we have to decode the assembly first, before we can make any sense of this problem
// This is an insidious bit of code that likes to infinite loop, which makes it hard to tell if it will or won't halt
//
// #ip 2
// 00 | seti 123 - 4        | r4 = 123       |
// 01 | bani 4 456 4        | r4 &= 456      | Tests that 123 & 456 == 72
// 02 | eqri 4 72 4         | r4 = r4 == 72  | Infinite loop if not
// 03 | addr 4 2 2          | ip += r4       |
// 04 | seti 0 - 2          | ip = 01        /
// 05 | seti 0 - 4          | r4 = 0                          \ 06:
// 06 | bori 4 65536 3      | r3 = r4 | 65536                 |    r3 = r4 | 65536
// 07 | seti #X - 4         | r4 = #X                         |    r4 = #X
// 08 | bani 3 255 1        | r1 = r3 & 255                   | 08: 
// 09 | addr 4 1 4          | r4 += r1                        |    r4 = (((r4 + (r3 & 255)) & 16777215) * 65899) & 16777215
// 10 | bani 4 16777215 4   | r4 &= 16777215                  |
// 11 | muli 4 65899 4      | r4 *= 65899                     |                             \ loop {
// 12 | bani 4 16777215 4   | r4 &= 16777215                  | if 256 > r3:                |   r3 = r4 | 65536
// 13 | gtir 256 3 1        | r1 = 256 > r3  \                |   if r4 == r0:              |   r4 = #X
// 14 | addr 1 2 2          | ip += r1       | if 256 > r3:   |     exit                    |   loop {
// 15 | addi 2 1 2          | ip = 17        |     goto 28 ---+   goto 05                   |     r4 = ...
// 16 | seti 27 - 2         | ip = 28        /                |                             |     if 256 > r3:
// 17 | seti 0 - 1          | r1 = 0                          | r1 = 0                      |       if r4 == r0:
// 18 | addi 1 1 5          | r5 = r1 + 1                     | loop {                      |         exit
// 19 | muli 5 256 5        | r5 *= 256                       |   if (r1 + 1) * 256 > r3:   |     else:
// 20 | gtrr 5 3 5          | r5 = r5 > r3   \ if r5 > r3:    |     r3 = r1                 |       break
// 21 | addr 5 2 2          | ip += r5       |     goto 26    |     goto 08                 |     r3 /= 256
// 22 | addi 2 1 2          | ip = 24        | else:          |   else:                     |   }
// 23 | seti 25 - 2         | ip = 26        /     goto 24    |     r1++                    | }
// 24 | addi 1 1 1          | r1++                            | }                           /
// 25 | seti 17 - 2         | ip = 18                         |
// 26 | setr 1 - 3          | r3 = r1                         |
// 27 | seti 7 - 2          | ip = 08                         |
// 28 | eqrr 4 0 1          | r1 = r4 == r0  \ if r4 == r0: <-+
// 29 | addr 1 2 2          | ip += r1       |     exit
// 30 | seti 5 - 2          | ip = 05        / goto 06
//
// From this, we can identify the core 'loop' of the program:
// N.B. we map r3 := a, and r4 := b
//
// a = b = 0
// loop {
//   a = b | 65536
//   b = #X
//   loop {
//     b = ...
//     if 256 > a:
//       if b == r0:   <--+ In order to trigger this as fast as possible (part 1), we need to compute the first value
//         exit            | of r4 that is reached in this loop 
//     else:               | For the longest, non-infinite value (part 2), we need to run this loop until we see a repeat
//       break             | At that point, we're in an infinite loop, and won't ever halt.
//     a /= 256            | The answer is the last unique value
//   }
// }

#include "aoc.h"

typedef struct { int a, b; } state_t;

#define state_t(_a, _b) ((state_t) { .a = (_a), .b = (_b) })

#define set_value_t state_t
#define set_equal(x, y) ((x).a == (y).a && (x).b == (y).b)
#define set_hash(x) ((x).a ^ (x).b)
#define set_empty ((state_t) { .a = -1, .b = -1 })
#include "../lib/set.h"

#define vec_data_t int
#include "../lib/vec.h"


#define SIZE 31

typedef struct {
    char opcode[5];
    int a, b, c;
} instruction_t;


main {
    // This parsing is almost entirely unnecessary, but it is simple, and makes sure we get everything parsed correctly
    int rip;
    instruction_t* code = (instruction_t*) malloc(sizeof(instruction_t) * SIZE);

    assert(scanf("#ip %d\n", &rip) == 1, "reading #ip");
    for (int i = 0; i < SIZE; i++) {
        instruction_t* inst = &code[i];

        assert(scanf("%4s %d %d %d\n", inst->opcode, &inst->a, &inst->b, &inst->c) == 4, "reading line %d", i);
    }

    int salt = code[7].a;
    free(code); // Done with parsed inputs

    // Part 1:
    // Iterate until we reach the first halting point
    state_t state = state_t(65536, salt);
    loop {
        state.b = (((state.b + (state.a & 255)) & 16777215) * 65899) & 16777215;
        if (256 > state.a) break;
        state.a /= 256;
    }

    println("Part 1: %d", state.b);

    // Part 2:
    // First, we need to iterate until we find a single repeated *state* (a pair of a,b)
    // As we do so, we are storing the b values in a vector, in parallel
    // Then, we need to find the value in b, which is the last unique value to appear
    // Effectively, find the minimum value in the array, sorted by the index of the first occurrence of that value
    
    vec_t* order = vec_new();
    set_t* seen = set_new();

    state = state_t(0, 0);
    loop {
        state.a = state.b | 65536;
        state.b = salt;
        loop {
            state.b = (((state.b + (state.a & 255)) & 16777215) * 65899) & 16777215;
            if (256 > state.a) break;
            state.a /= 256;
        }
        if (set_contains(seen, state)) {
            break;
        }

        int b = state.b;
        set_insert(seen, state);
        vec_push(order, b);
    }

    // This is a hack, but we don't have generic types. So this is a `set<int>` now for tracking unique types
    set_clear(seen);

    int unique = -1;
    for (int i = 0; i < order->len; i++) {
        int value = vec_get(order, i);
        if (!set_insert(seen, state_t(0, value))) {
            unique = value;
        }
    }

    println("Part 2: %d", unique);
}
