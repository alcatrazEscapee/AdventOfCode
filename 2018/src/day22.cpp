#include "aoc.h"
#include "../lib/point.h"

// Points for the four adjacent cardinal directions. Immutable.
static const Point CARDINALS[4] = { Point(0, -1), Point(-1, 0), Point(1, 0), Point(0, 1) };

enum Type { Rocky, Wet, Narrow, Invalid };
enum Tool { Torch, ClimbingGear, None };

class Cave {
public:
    Cave(Point target, int depth) : target(target), depth(depth) {} 

    Type get_type(const Point& pos) {
        return static_cast<Type>(get_erosion_level(pos) % 3);
    }

    // Heuristic used in A*
    int get_dist_to_target(const Point& pos, const Tool tool) {
        return std::abs(pos.x - target.x) + std::abs(pos.y - target.y) + (tool == Torch ? 0 : 7);
    }

private:
    int get_geologic_index(const Point& pos) {
        if (pos == this->target) return 0;
        if (pos.y == 0) return pos.x * 16807;
        if (pos.x == 0) return pos.y * 48271;
        
        // Use an int key, because we're fairly confident positions aren't above 65535, and it's more memory dense
        int key = (pos.x << 16) | pos.y;
        int& index = cache[key];

        if (index != 0) return index;

        int new_index = get_erosion_level(Point(pos.x - 1, pos.y)) * get_erosion_level(Point(pos.x, pos.y - 1));
        index = new_index; // Enters it into the cache, due to this being a reference, without an additional lookup
        return index;
    }

    int get_erosion_level(const Point& pos) {
        return (get_geologic_index(pos) + this->depth) % 20183;
    }

    std::unordered_map<int, int> cache;
public:
    Point target;
private:
    int depth;
};

int pack_key(const Point& pos, const Tool& tool);


class Path {
public:
    Path() = default;
    Path(Point pos, Tool tool, int cost) : pos(pos), tool(tool), cost(cost) {}

    int key() { return pack_key(pos, tool); }

    Point pos;
    Tool tool;
    int cost;
};


bool is_compatible(Type type, Tool tool);
Tool get_compatible_tool(Type type, Tool tool);


main {
    Point target;
    int depth;

    assert(scanf("depth: %d\n", &depth) == 1, "reading depth");
    assert(scanf("target: %d,%d\n", &target.x, &target.y) == 2, "reading target");

    Cave* cave = new Cave(target, depth);

    // Part 1 is fairly straightforward - we use DP (memoization) in order to make the computation of the geologic index fast
    // Then just sum up the square.
    int risk = 0;
    for (int x = 0; x <= target.x; x++) {
        for (int y = 0; y <= target.y; y++) {
            risk += static_cast<int>(cave->get_type(Point(x, y)));
        }
    }

    println("Part 1: %d", risk);

    // Part 2 is a slightly more involved BFS
    // First, we need some custom operators
    struct Compare {
        bool operator()(const Path& a, const Path& b) {
            return a.cost > b.cost;
        }
    };

    // For the search, we use A*
    // This was an order-of-magnitude improvement over using a standard Dijkstra's in this problem (~22s -> ~2s).
    // Some refactors (using `[]` instead of `.find()`), and optimizations (using `pack_key` and a `int` based cost map),
    // yielded an improvement to ~0.5s runtime.
    std::priority_queue<Path, std::vector<Path>, Compare> queue;
    std::unordered_map<int, int> costs;

    queue.push(Path(Point(), Torch, cave->get_dist_to_target(Point(), Torch)));

    while (queue.size() > 0) {
        Path top = queue.top();
        queue.pop();

        int cost = costs[top.key()];

        if (top.pos == cave->target && top.tool == Torch) {
            println("Part 2: %d", cost);
            break;
        }

        for (const Point& adj : CARDINALS) {
            Point next = top.pos + adj;
            if (next.x >= 0 && next.y >= 0) {
                Type next_type = cave->get_type(next);
                if (is_compatible(next_type, top.tool)) {
                    int next_cost = cost + 1;
                    int& prev_cost = costs[pack_key(next, top.tool)];
                    if (prev_cost == 0 || next_cost < prev_cost) { // If we haven't seen this point, or can visit it faster
                        queue.push(Path(next, top.tool, next_cost + cave->get_dist_to_target(next, top.tool)));
                        prev_cost = next_cost;
                    }
                }
            }
        }

        Tool swap = get_compatible_tool(cave->get_type(top.pos), top.tool);
        int& prev_cost = costs[pack_key(top.pos, swap)];
        int next_cost = cost + 7;
        if (prev_cost == 0 || next_cost < prev_cost) {
            queue.push(Path(top.pos, swap, next_cost + cave->get_dist_to_target(top.pos, swap)));
            prev_cost = next_cost;
        }
    }
}


bool is_compatible(Type type, Tool tool) {
    return !((type == Rocky && tool == None)
        || (type == Wet && tool == Torch)
        || (type == Narrow && tool == ClimbingGear));
}

Tool get_compatible_tool(Type type, Tool tool) {
    return type == Rocky ? (tool == ClimbingGear ? Torch : ClimbingGear)
        : type == Wet ? (tool == ClimbingGear ? None : ClimbingGear)
        : (tool == Torch ? None : Torch);
}

int pack_key(const Point& pos, const Tool& tool) {
    return (pos.x << 14) | (pos.y << 2) | static_cast<int>(tool);
}
