#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define main int main(int argc, slice_t* argv)
#define assert(x) _Static_assert(x, #x)
#define read_input(inp, len) \
    slice_t inp = NULL; \
    size_t len; \
    do { \
        size_t buf = 0; \
        len = getline(&inp, &buf, stdin); \
    } while (0)

#define slice_t char*
#define pointer_t void*
#define byte_t char

assert(sizeof(byte_t) == 1);

#define new(type) type ## _new
#define copy(type) type ## _copy
#define del(x) free(x)
#define loop while (1)

#define slice_t_new(n) pointer_t_new((n) + 1)
#define slice_t_copy(x) ((slice_t)strdup(x))

pointer_t pointer_t_new(size_t n) {
    pointer_t p = (pointer_t) malloc(n);
    if (p == NULL) {
        printf("Memory allocation of %d bytes failed\n", n);
        exit(1);
    }
    memset(p, 0, n);
    return p;
}