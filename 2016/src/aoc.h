#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define read_in(inp, len) NULL; do { size_t buf = 0; len = getline(&inp, &buf, stdin); } while (0)
#define args int argc, slice_t* argv

#define slice_t char*
#define pointer_t void*
#define int_t int32_t*

#define new(type) type ## _new
#define copy(type) type ## _copy
#define slice_t_copy(x) ((slice_t)strdup(x))
#define del(x) free(x)
#define loop while (1)

#define slice_t_new(n) pointer_t_new(sizeof(char) * (n) + 1)
#define int_t_new(n) pointer_t_new(sizeof(int32_t) * (n))

pointer_t pointer_t_new(size_t n) {
    pointer_t p = (pointer_t) malloc(n);
    memset(p, 0, n);
    return p;
}
