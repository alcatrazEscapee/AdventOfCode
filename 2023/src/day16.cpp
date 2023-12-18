#include "aoc.h"

#define SIZE 110

typedef unsigned char u8;
static_assert(sizeof(u8) == 1);


enum class Tile : u8 {
    Empty = 0,
    MirrorP = 1,
    MirrorN = 2,
    SplitV = 3,
    SplitH = 4,
};

enum class Direction : u8 {
    North = 0,
    East  = 1,
    South = 2,
    West  = 3,

    Horizontal = 4,
    Vertical = 5
};


class Point {
public:

    Point() = default;
    Point(Direction dir):
        x(dir == Direction::East ? 1 : dir == Direction::West ? -1 : 0),
        y(dir == Direction::South ? 1 : dir == Direction::North ? -1 : 0) {}
    Point(u8 x, u8 y) : x(x), y(y) {}

    Point& operator+=(const Point& rhs) { x += rhs.x; y += rhs.y; return *this; }
    Point& operator-=(const Point& rhs) { x -= rhs.x; y -= rhs.y; return *this; }

    const Point operator+(const Point& other) const { return Point(*this) += other; }
    const Point operator-(const Point& other) const { return Point(*this) -= other; }

    bool operator==(const Point& other) const = default;

    u8 x, y;
};


Tile from_char(char c);

size_t calculate_energy(
    const std::vector<Direction>& moves,
    const std::vector<Tile>& grid,
    const std::pair<Point, Direction> start
);


main {
    // A collapsed map of all result directions, given an input direction and tile.
    // This is much more efficient and compact than a std::unordered_map
    //
    // Accessed by (tile << 2) | dir
    std::vector<Direction> moves = {
        // North               East                 South                  West
        Direction::North,      Direction::East,     Direction::South,      Direction::West,     // Empty
        Direction::East,       Direction::North,    Direction::West,       Direction::South,    // MirrorP
        Direction::West,       Direction::South,    Direction::East,       Direction::North,    // MirrorN
        Direction::North,      Direction::Vertical, Direction::South,      Direction::Vertical, // SplitV
        Direction::Horizontal, Direction::East,     Direction::Horizontal, Direction::West,     // SplitH
    };

    std::vector<Tile> grid;
    std::string line;    
    
    grid.reserve(SIZE * SIZE);

    while (std::getline(std::cin, line)) {
        for (const auto c : line) {
            grid.push_back(from_char(c));
        }
    }

    assert(grid.size() == SIZE * SIZE, "Wrong size grid, expected %d x %d, got %lld", SIZE, SIZE, grid.size());

    size_t part1 = calculate_energy(moves, grid, std::pair(Point(-1, 0), Direction::East));
    size_t part2 = 0;

    for (int n = 0; n < SIZE; n++) {
        part2 = std::max(part2, calculate_energy(moves, grid, std::pair(Point(-1, n), Direction::East)));
        part2 = std::max(part2, calculate_energy(moves, grid, std::pair(Point(SIZE, n), Direction::West)));
        part2 = std::max(part2, calculate_energy(moves, grid, std::pair(Point(n, -1), Direction::South)));
        part2 = std::max(part2, calculate_energy(moves, grid, std::pair(Point(n, SIZE), Direction::North)));
    }

    std::cout
        << "Part 1: " << part1 << std::endl
        << "Part 2: " << part2 << std::endl;
}

Tile from_char(char c) {
    switch (c) {
        case '/': return Tile::MirrorP;
        case '\\': return Tile::MirrorN;
        case '|': return Tile::SplitV;
        case '-': return Tile::SplitH;
        default: return Tile::Empty;
    }
}

size_t calculate_energy(
    const std::vector<Direction>& moves,
    const std::vector<Tile>& grid,
    const std::pair<Point, Direction> start
) {
    std::vector<std::pair<Point, Direction>> queue = { start };
    std::vector<u8> seen;  // Bitmask of used directions at each given position (x + SIZE * y)

    seen.reserve(SIZE * SIZE);
    for (int i = 0; i < SIZE * SIZE; i++) {
        seen.push_back(0);
    }

    while (!queue.empty()) {
        auto [pos, dir] = queue.back();
        queue.pop_back();

        loop {
            pos += dir;

            if (pos.x < 0 || pos.y < 0 || pos.x >= SIZE || pos.y >= SIZE) {
                break; // Out of bounds
            }

            const size_t index = pos.x + SIZE * pos.y;
            const u8 bit = 1 << static_cast<u8>(dir);

            u8& key = seen[index];
            if ((key & bit) != 0) {
                break; // Already found this position
            }
            key |= bit;

            const Tile tile = grid[index];
            const size_t moveIndex = (static_cast<u8>(tile) << 2) | static_cast<u8>(dir);
            const Direction move = moves.at(moveIndex);

            switch (move) {
                case Direction::Vertical:
                    dir = Direction::North;
                    queue.push_back(std::pair(pos, Direction::South));
                    break;
                case Direction::Horizontal:
                    dir = Direction::West;
                    queue.push_back(std::pair(pos, Direction::East));
                    break;
                default:
                    dir = move;
                    break;
            }
        }
    }

    size_t n = 0;
    for (u8 bit : seen) {
        if (bit != 0) {
            n++;
        }
    }

    return n;
}