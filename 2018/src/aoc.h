#ifdef __cplusplus

#include <iostream>
#include <cstdio>
#include <algorithm>
#include <string>
#include <vector>
#include <queue>
#include <deque>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <bit>

#else

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>
#include <math.h>

#define static_assert _STATIC_ASSERT

#endif // __cplusplus

#ifndef AOC_H
#define AOC_H

#define main \
void __main__(); \
int main(int argc, char** argv) {\
    __main__(); \
} \
void __main__()

/// Declares an integral (hence `i` type) type, and statically asserts it has the assumed amount of bytes
/// Used for Rust-like declarations, like `itype(int, 4) i32;`
#define itype(root_type, bytes) static_assert(sizeof(root_type) == bytes) ; typedef root_type 

/// Like `printf` but appends a newline at the end, since we pretty much always want this
#define println(format_string, ...) printf(format_string "\n" , ## __VA_ARGS__)

/// Assertions that are able to print a formatted string if the assertion fails.
/// Also includes line and file information
#define assert(condition, format_string, ...) do { \
    if (!(condition)) { \
        println("Assertion Failed: " format_string, ## __VA_ARGS__); \
        println("    at: %s", #condition); \
        println("    at: %s:%d", __FILE__, __LINE__); \
        exit(-1); \
    } \
} while (0)

/// An auto-exit with error information for unreachable code, i.e. in a switch default case. Modeled after Rust's `unreachable!()` macro.
#define unreachable do { \
    println("Unreachable code reached!"); \
    println("    at: %s:%d", __FILE__, __LINE__); \
    exit(-1); \
} while (0)

#define loop while(1)

#endif