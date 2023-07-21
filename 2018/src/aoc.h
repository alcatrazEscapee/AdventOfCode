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

#else

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

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

typedef unsigned char byte;

#define println(format_string, ...) printf(format_string "\n" , ## __VA_ARGS__)

#define assert(condition, format_string, ...) do { \
    if (!(condition)) { \
        println("Assertion Failed: " format_string, ## __VA_ARGS__); \
        println("    at: %s", #condition); \
        println("    at: %s:%d", __FILE__, __LINE__); \
        exit(-1); \
    } \
} while (0)

#define unreachable do { \
    println("Unreachable code!"); \
    println("    at: %s:%d", __FILE__, __LINE__); \
    exit(-1); \
} while (0)

#define loop while(1)

#endif