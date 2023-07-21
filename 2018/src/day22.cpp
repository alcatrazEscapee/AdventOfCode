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
        if (pos == Point(0, 0) || pos == this->target) return 0;
        if (pos.y == 0) return pos.x * 16807;
        if (pos.x == 0) return pos.y * 48271;
        
        auto ptr = cache.find(pos);
        if (ptr != cache.end()) return ptr->second;

        int index = get_erosion_level(Point(pos.x - 1, pos.y)) * get_erosion_level(Point(pos.x, pos.y - 1));
        cache[pos] = index;
        return index;
    }

    int get_erosion_level(const Point& pos) {
        return (get_geologic_index(pos) + this->depth) % 20183;
    }

    std::unordered_map<Point, int, Point::Hash> cache;
public:
    Point target;
private:
    int depth;
};


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

    struct Hash {
        size_t operator()(const std::pair<Point, Tool>& pair) const {
            return (Point::Hash()(pair.first)) ^ (std::hash<int>()(pair.second));
        }
    };

    struct Compare {
        bool operator()(const std::tuple<Point, Tool, int>& a, const std::tuple<Point, Tool, int>& b) {
            return std::get<2>(a) > std::get<2>(b);
        }
    };

    // For the search, we use A*
    // This was an order-of-magnitude improvement over using a standard Dijkstra's in this problem (~22s -> ~2s).
    std::priority_queue<std::tuple<Point, Tool, int>, std::vector<std::tuple<Point, Tool, int>>, Compare> queue;
    std::unordered_map<std::pair<Point, Tool>, int, Hash> costs;

    queue.push(std::tuple(Point(0, 0), Torch, cave->get_dist_to_target(Point(0, 0), Torch)));

    while (queue.size() > 0) {
        std::tuple<Point, Tool, int> top = queue.top();
        queue.pop();

        auto& [ pos, tool, _ ] = top;
        int cost = costs[std::pair(pos, tool)];

        if (pos == cave->target && tool == Torch) {
            println("Part 2: %d", cost);
            break;
        }

        for (const Point& adj : CARDINALS) {
            Point next = pos + adj;
            if (next.x >= 0 && next.y >= 0 && is_compatible(cave->get_type(next), tool)) {
                int next_cost = cost + 1;
                auto ptr = costs.find(std::pair(next, tool));
                if (ptr == costs.end() || next_cost < ptr->second) { // If we haven't seen this point, or can visit it faster
                    queue.push(std::tuple(next, tool, next_cost + cave->get_dist_to_target(next, tool)));
                    costs[std::pair(next, tool)] = next_cost;
                }
            }
        }

        Tool swap = get_compatible_tool(cave->get_type(pos), tool);
        auto ptr = costs.find(std::pair(pos, swap));
        int next_cost = cost + 7;
        if (ptr == costs.end() || next_cost < ptr->second) {
            queue.push(std::tuple(pos, swap, next_cost + cave->get_dist_to_target(pos, swap)));
            costs[std::pair(pos, swap)] = next_cost;
        }
    }
}
