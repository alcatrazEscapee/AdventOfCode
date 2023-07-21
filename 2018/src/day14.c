#include "aoc.h"

#define LENGTH 10

typedef unsigned long long int u64;
typedef unsigned int u32;
typedef unsigned char u8;

static_assert(sizeof(u64) == 8);
static_assert(sizeof(u32) == 4);
static_assert(sizeof(u8) == 1);

#define vec_data_t u8
#define vec_t u8_vec_t
#include "../lib/vec.h"


bool check_recipe(u8_vec_t* recipes, u8* digits, u32 len, u8 offset);


main {
    u32 input;
    u8_vec_t* recipes = vec_new();
    u32 elf1 = 0, elf2 = 1;
    
    assert(scanf("%d\n", &input) == 1, "reading input");
    vec_push2(recipes, 3, 7);

    // 'search' will be input digits, in reverse order
    // 'length' will contain the number of digits
    u8 search[LENGTH] = { 0 }, length = 0, *ptr = search;
    for (u32 n = input; n > 0; n /= 10, ptr++, length++) {
        *ptr = n % 10;
    }

    bool part1 = false, part2 = false;

    while (!part1 || !part2) {
        u8 value1 = vec_get(recipes, elf1),
           value2 = vec_get(recipes, elf2);
        u8 mix = value1 + value2;

        if (mix >= 10) vec_push(recipes, mix / 10);
        vec_push(recipes, mix % 10);

        elf1 = (elf1 + 1 + value1) % recipes->len;
        elf2 = (elf2 + 1 + value2) % recipes->len;

        // Part 1: After 'input' recipes, what is the last ten digits
        if (recipes->len == 10 + input) {
            part1 = true;

            char answer[LENGTH + 1];
            for (int i = 0; i < LENGTH; i++) {
                answer[i] = '0' + vec_get(recipes, recipes->len - LENGTH + i);
            }

            println("Part 1: %s", answer);
        }

        // Part 2: How many steps to get a recipe that matches the input digits
        if (check_recipe(recipes, search, length, 0)) {
            println("Part 2: %d", recipes->len - length);
            part2 = true;
        }
        if (check_recipe(recipes, search, length, 1)) {
            println("Part 2: %d", recipes->len - length - 1);
            part2 = true;
        }
    }

}

bool check_recipe(u8_vec_t* recipes, u8* digits, u32 len, u8 offset) {
    for (int i = 0; i < len; i++)
        if (vec_get(recipes, recipes->len - i - offset - 1) != digits[i])
            return false;
    return true;
}
