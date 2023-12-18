#ifndef AOC_H
#define AOC_H

#include <unordered_map>
#include <unordered_set>
#include <iostream>
#include <cstdio>
#include <string>
#include <vector>

#define main \
void __main__(); \
int main(int argc, char** argv) {\
    __main__(); \
} \
void __main__()

#define loop while(1)

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

#endif