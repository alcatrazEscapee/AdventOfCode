// A very basic resizable `Vec<T>` implementation.
// Supports push(), get(), new(), clear() operations.
//
// Can be defined with `vec_data_t` and `vec_t`
// Not properly re-entrant - cannot be included twice with different types

#ifndef VEC_H
#define VEC_H

#ifndef vec_data_t
#define vec_data_t int
#endif

typedef struct {
    vec_data_t* data;
    int capacity;
    int len;
} vec_t;

#define vec_push2(vec, a, b) vec_push(vec, a), vec_push(vec, b)

vec_t* vec_new() {
    vec_t* vec = (vec_t*) malloc(sizeof(vec_t));
    memset(vec, 0, sizeof(vec_t));
    return vec;
}

void vec_clear(vec_t* vec) {
    vec->len = 0;
}

void vec_push(vec_t* vec, vec_data_t value) {
    if (vec->len >= vec->capacity) {
        vec->capacity *= 2;
        if (vec->capacity == 0) vec->capacity = 8;
        vec->data = realloc(vec->data, vec->capacity);
    }
    vec->data[vec->len] = value;
    vec->len++;
}

vec_data_t vec_get(vec_t* vec, int index) {
    assert(index < vec->len, "Index out of bounds: %d not in range [0, %d)", index, vec->len);
    return vec->data[index];
}

#endif