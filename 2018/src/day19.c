// Examining the structure of the input, we can identify the core section of code
//
// 01 | seti 1 3 5     | r5 = 1 <---------------------------+ r0 = 0
// 02 | seti 1 1 3     | r3 = 1                             | r2 = <some large number>
// 03 | mulr 5 3 1     | r1 = r3 * r5                 <--+  | r3 = r5 = 1
// 04 | eqrr 1 2 1     | r1 = r1 == r2  \                |  | do {
// 05 | addr 1 4 4     | r4 += r1       | if r1 != r2:   |  |     do {
// 06 | addi 4 1 4     | r4 += 1        |     r0 += r5   |  |         if r3 * r5 != r2:
// 07 | addr 5 0 0     | r0 += r5       /                |  |             r0 += r5
// 08 | addi 3 1 3     | r3 += 1                         |  |         r3++
// 09 | gtrr 3 2 1     | r1 = r3 > r2   \ if r3 <= r2:   |  |     } while (r3 <= r2)
// 10 | addr 4 1 4     | r4 += r1       |     goto 03 ---+  |     r5++
// 11 | seti 2 8 4     | r4 = 2         /                   | } while (r5 <= r2)
// 12 | addi 5 1 5     | r5 += 1                            | exit
// 13 | gtrr 5 2 1     | r1 = r5 > r2   \ if r5 <= r2:      | 
// 14 | addr 1 4 4     | r4 += r1       |     goto 01 ------+ > For a given r2, it's computing factors
// 15 | seti 1 3 4     | r4 = 1         | else:               > r0 is the sum of all factors of r2
// 16 | mulr 4 4 4     | exit           /     exit            > This is O(n^2), we can do it  in O(sqrt(n))
//
// The rest of the code is two different initializations for r2, one for part 1, and one for part 2
// Obviously, the part 2 value is much larger.
// Here, we run the code entirely for part 1 (because it's feasible), and stop it before the slow,
// O(n^2) loop in part 2, and break into a native solution instead.

#include "aoc.h"

#define SIZE 36

typedef struct {
    int opcode;
    int a, b, c;
} instruction_t;

typedef enum {
    AddR, AddI, MulR, MulI, AndR, AndI, OrR, OrI, SetR, SetI, GtIR, GtRI, GtRR, EqIR, EqRI, EqRR, NumOpcodes
} opcode_t;

static char* OPCODES[NumOpcodes] = { "addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr" };

void i_exec(instruction_t* inst, int reg[6]);


main {
    int rip;
    instruction_t* code = (instruction_t*) malloc(sizeof(instruction_t) * SIZE);

    assert(scanf("#ip %d\n", &rip) == 1, "reading #ip");
    for (int i = 0; i < SIZE; i++) {
        char mnemonic[5] = { 0 };
        instruction_t* inst = &code[i];

        assert(scanf("%4s %d %d %d\n", mnemonic, &inst->a, &inst->b, &inst->c) == 4, "reading line %d", i);

        for (opcode_t op = 0; op < NumOpcodes; op++) {
            if (strcmp(mnemonic, OPCODES[op]) == 0) {
                inst->opcode = op;
                break;
            }
        }
    }

    int ip = 0;
    int reg[6] = { 0 };

    for (; ip >= 0 && ip < SIZE; ip++) {
        reg[rip] = ip;
        i_exec(&code[ip], reg);
        ip = reg[rip];
    }

    println("Part 1: %d", reg[0]);

    ip = 0;
    memset(reg, 0, sizeof(reg));
    reg[0] = 1;

    // Only loop until we hit instruction 01
    for (; ip != 1; ip++) {
        reg[rip] = ip;
        i_exec(&code[ip], reg);
        ip = reg[rip];
    }

    // And then do the rest of the solution using modulo, instead of a multiplication loop
    // Slight optimization, by assuming the answer is not a perfect square, and we can skip it's factors
    int r2 = reg[2], sum = 0;
    for (int f = 1; f <= sqrt(r2); f++) {
        if (r2 % f == 0) {
            sum += f + r2 / f;
        }
    }

    println("Part 2: %d", sum);
}


void i_exec(instruction_t* inst, int reg[6])
{

#define RA reg[inst->a]
#define RB reg[inst->b]
#define RC reg[inst->c]

#define IA inst->a
#define IB inst->b

    switch (inst->opcode) {
        case AddR: RC = RA + RB; break;
        case AddI: RC = RA + IB; break;
        case MulR: RC = RA * RB; break;
        case MulI: RC = RA * IB; break;
        case AndR: RC = RA & RB; break;
        case AndI: RC = RA & IB; break;
        case OrR:  RC = RA | RB; break;
        case OrI:  RC = RA | IB; break;
        case SetR: RC = RA;      break;
        case SetI: RC = IA;      break;
        case GtIR: RC = IA > RB; break;
        case GtRI: RC = RA > IB; break;
        case GtRR: RC = RA > RB; break;
        case EqIR: RC = IA == RB; break;
        case EqRI: RC = RA == IB; break;
        case EqRR: RC = RA == RB; break;
        default: unreachable;
    }
}