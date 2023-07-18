#include "aoc.h"

#define LENGTH 10

typedef unsigned long long int u64;
typedef unsigned int u32;
typedef unsigned char u8;


// A very basic resizable Vector<u8> implementation
// Supports push(), get(), new(), clear() operations.
typedef struct {
    u8* data;
    u32 capacity;
    u32 len;
} u8_vec_t;

u8_vec_t* vec_new();

void vec_clear (u8_vec_t* vec);
void vec_push  (u8_vec_t* vec, u8 value);
u8   vec_get   (u8_vec_t* vec, u32 index);

#define vec_push2(vec, a, b) vec_push(vec, a), vec_push(vec, b)


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


// === Vector<u8> implementation ===

u8_vec_t* vec_new() {
    u8_vec_t* vec = (u8_vec_t*) malloc(sizeof(u8_vec_t));
    memset(vec, 0, sizeof(u8_vec_t));
    return vec;
}

void vec_clear(u8_vec_t* vec) {
    vec->len = 0;
}

void vec_push(u8_vec_t* vec, u8 value) {
    if (vec->len >= vec->capacity) {
        vec->capacity *= 2;
        if (vec->capacity == 0) vec->capacity = 8;
        vec->data = realloc(vec->data, vec->capacity);
    }
    vec->data[vec->len] = value;
    vec->len++;
}

u8 vec_get(u8_vec_t* vec, u32 index) {
    assert(index < vec->len, "Index out of bounds: %d not in range [0, %d)", index, vec->len);
    return vec->data[index];
}
