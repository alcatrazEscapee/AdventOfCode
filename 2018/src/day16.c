#include "aoc.h"

#define CASES 774
#define CODE 956


typedef struct {
    int opcode;
    int a, b, c;
} instruction_t;

typedef struct {
    int before[4];
    int after[4];
    instruction_t inst;
} case_t;

typedef enum {
    AddR, AddI, MulR, MulI, AndR, AndI, OrR, OrI, SetR, SetI, GtIR, GtRI, GtRR, EqIR, EqRI, EqRR, NumOpcodes
} opcode_t;


bool i_test(case_t* c, opcode_t opcode);
void i_exec(instruction_t* inst, int reg[4]);


main  {
    case_t* cases = (case_t*) malloc(sizeof(case_t) * CASES);
    instruction_t* instructions = (instruction_t*) malloc(sizeof(instruction_t) * CODE);

    for (int i = 0; i < CASES; i++) {
        case_t* c = &cases[i];

        assert(scanf("Before: [%d, %d, %d, %d]\n", &c->before[0], &c->before[1], &c->before[2], &c->before[3]) == 4, "reading line %d", i);
        assert(scanf("%d %d %d %d\n", &c->inst.opcode, &c->inst.a, &c->inst.b, &c->inst.c) == 4, "reading line %d", i);
        assert(scanf("After: [%d, %d, %d, %d]\n", &c->after[0], &c->after[1], &c->after[2], &c->after[3]) == 4, "reading line %d", i);
        assert(scanf("\n") == 0, "reading line %d", i);
    }

    assert(scanf("\n\n") == 0, "reading separator");

    for (int i = 0; i < CODE; i++) {
        instruction_t* inst = &instructions[i];

        assert(scanf("%d %d %d %d\n", &inst->opcode, &inst->a, &inst->b, &inst->c) == 4, "reading instruction line %d", i);
    }

    // Part 1: how many samples behave like three or more opcodes
    // No better way to do this than to just test each opcode against each sample
    // We can simplify a bit, by stopping if we already can match >= 3
    int part1 = 0;
    for (int i = 0; i < CASES; i++) {
        case_t* c = &cases[i];
        int matches = 0;
        for (opcode_t op = 0; op < NumOpcodes; op++) {
            if (i_test(c, op)) {
                matches++;
            }
            if (matches >= 3) {
                part1++;
                break;
            }
        }
    }

    println("Part 1: %d", part1);

    // Part 2: We need to build the mapping of opcode # -> opcode_t
    opcode_t mapping[NumOpcodes]; // opcode # -> opcode_t
    opcode_t mapping_inv[NumOpcodes]; // opcode_t -> opcode #
    opcode_t open = NumOpcodes; // Number of open = unassigned opcodes remaining

    memset(mapping, -1, sizeof(opcode_t) * NumOpcodes); // Fill the array initially with -1
    memset(mapping_inv, -1, sizeof(opcode_t) * NumOpcodes);

    while (open > 0) {
        for (int i = 0; i < CASES; i++) { // Iterate through cases in a loop, until we find one that identifies an unknown opcode
            case_t* c = &cases[i];
            opcode_t match = -1;

            // If this instruction's opcode # has already been mapped, then there's no point in us re-trying it.
            if (mapping[c->inst.opcode] != -1) continue;
            
            for (opcode_t op = 0; op < NumOpcodes; op++) {
                if (mapping_inv[op] == -1 && i_test(c, op)) { // If we have not already mapped an opcode # to this opcode_t
                    if (match == -1) {
                        match = op; // First match
                    } else {
                        match = -1; // More than one matches, so break and set back to -1
                        break;
                    }
                }
            }

            if (match != -1) {
                mapping[c->inst.opcode] = match; // Record the mapping from instruction opcode # to opcode_t
                mapping_inv[match] = c->inst.opcode; // And inverse
                open--; // Decrement the remaining open opcodes
                break; // Restart back at the beginning
            }
        }
    }

    // Now we should be able to execute the program
    // First, remap all instructions to use the correct opcode
    for (int i = 0; i < CODE; i++) {
        instructions[i].opcode = mapping[instructions[i].opcode];
    }

    // Then execute, top to bottom
    // This language has no jumps (yet), so a simple loop will do
    int reg[4] = { 0 };
    for (int ip = 0; ip < CODE; ip++) {
        i_exec(&instructions[ip], reg);
    }

    println("Part 2: %d", reg[0]);
}


bool i_test(case_t* c, opcode_t opcode) {
    int reg[4];
    int op = c->inst.opcode;

    c->inst.opcode = opcode;
    memcpy(reg, c->before, 4 * sizeof(int));
    i_exec(&c->inst, reg);
    c->inst.opcode = op;

    return reg[0] == c->after[0]
        && reg[1] == c->after[1]
        && reg[2] == c->after[2]
        && reg[3] == c->after[3];
}


void i_exec(instruction_t* inst, int reg[4])
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