#include <unordered_map>
#include <iostream>
#include <cstdio>
#include <string>
#include <vector>

#define SIZE 100

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


enum class Type : unsigned char {
    Empty, Wall, Boulder
};

void zip_platform(std::vector<Type>& grid);
void tilt_platform(std::vector<Type>& grid);
void rotate_platform(const std::vector<Type>& src, std::vector<Type>& dest);

size_t load_platform(const std::vector<Type>& grid);


int main(int argc, char** argv) {

    std::vector<Type> grid;
    std::string line;

    grid.reserve(SIZE * SIZE);

    while (std::getline(std::cin, line)) {
        for (const auto c : line) {
            grid.push_back(c == '.' ? Type::Empty : c == '#' ? Type::Wall : Type::Boulder);
        }
    }

    assert(grid.size() == SIZE * SIZE, "Wrong size grid, expected %d x %d, got %lld", SIZE, SIZE, grid.size());

    zip_platform(grid);
    tilt_platform(grid);

    std::cout << "Part 1: " << load_platform(grid) << std::endl;


    struct Hash {
        int operator()(const std::vector<Type> &grid) const {
            int hash = grid.size();
            for (auto &i : grid) {
                hash = 31 * hash + static_cast<unsigned char>(i);
            }
            return hash;
        }
    };

    std::unordered_map<std::vector<Type>, size_t, Hash> seen;
    std::vector<Type> temp_grid = grid;

    for (size_t n = 0;; n++) {

        size_t& prev_n = seen[grid];
        if (prev_n != 0) {
            const size_t n_len = n - prev_n;
            const size_t n_offset = (1000000000 - n) % n_len + prev_n;

            for (auto& [grid, n] : seen) {
                if (n == n_offset) {
                    std::cout << "Part 2: " << load_platform(grid) << std::endl;
                    break;
                }
            }
            break;
        }

        // Rotate through four rotations, using the temp grid as a buffer
        tilt_platform(grid);
        rotate_platform(grid, temp_grid);
        
        tilt_platform(temp_grid);
        rotate_platform(temp_grid, grid);
        
        tilt_platform(grid);
        rotate_platform(grid, temp_grid);
        
        tilt_platform(temp_grid);
        rotate_platform(temp_grid, grid);

        prev_n = n;
    }

    return 0;
}

void zip_platform(std::vector<Type>& grid) {
    for (size_t y = 0; y < SIZE; y++) {
        for (size_t x = y; x < SIZE; x++) {
            std::swap(grid[x + SIZE * y], grid[y + SIZE * x]);
        }
    }
}

void rotate_platform(const std::vector<Type>& src, std::vector<Type>& dest) {
    for (size_t y = 0; y < SIZE; y++) {
        for (size_t x = 0; x < SIZE; x++) {
            dest[y + (SIZE - 1 - x) * SIZE] = src[x + y * SIZE];
        }
    }
}

void tilt_platform(std::vector<Type>& grid) {
    for (size_t y = 0; y < grid.size(); y += SIZE) {
        size_t n = y - 1;
        for (size_t i = y; i < y + SIZE; i++) {
            const Type c = grid[i];
            if (c == Type::Wall) {
                n = i;
            } else if (c == Type::Boulder) {
                grid[i] = Type::Empty;
                grid[n + 1] = Type::Boulder;
                n++;
            }
        }
    }
}

size_t load_platform(const std::vector<Type>& grid) {
    size_t load = 0;
    for (size_t y = 0; y < grid.size(); y += SIZE) {
        for (size_t i = y; i < y + SIZE; i++) {
            if (grid[i] == Type::Boulder) {
                load += SIZE - (i - y);
            }
        }
    }
    return load;
}