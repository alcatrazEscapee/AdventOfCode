#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define read_in(inp, len) NULL; do { size_t buf = 0; len = getline(&inp, &buf, stdin); } while (0)
#define args int argc, slice_t* argv

#define slice_t char*
#define new(type) type ## _new
#define copy(type) type ## _copy
#define slice_t_copy(x) ((slice_t)strdup(x))
#define del(x) free(x)

slice_t slice_t_new(size_t n) {
    slice_t str = (slice_t) malloc(sizeof(char) * n + 1);
    memset(str, '\0', n + 1);
    return str;
}