#include "aoc.h"

enum class Axis { X, Y };

class Edge {
public:
    Edge() = default;
    Edge(int x, int y, Axis axis) : x(x), y(y), axis(axis) {}

    bool operator==(const Edge& other) const { return x == other.x && y == other.y && axis == other.axis; }

    struct Hash {
        size_t operator()(const Edge& self) const {
            return (std::hash<int>()(self.x) << 1) ^ (std::hash<int>()(self.y)) ^ (self.axis == Axis::X ? 1 : 0);
        }
    };

    int x, y;
    Axis axis;
};

class Pos {
public:
    Pos() = default;
    Pos(int x, int y) : x(x), y(y) {}

    const Edge edge(char c) {
        switch (c) {
            case 'N': return Edge(x, y - 1, Axis::Y);
            case 'E': return Edge(x, y, Axis::X);
            case 'S': return Edge(x, y, Axis::Y);
            case 'W': return Edge(x - 1, y, Axis::X);
            default: unreachable;
        }
    }

    const Pos adj(char c) {
        switch (c) {
            case 'N': return Pos(x, y - 1);
            case 'E': return Pos(x + 1, y);
            case 'S': return Pos(x, y + 1);
            case 'W': return Pos(x - 1, y);
            default: unreachable;
        }
    }

    bool operator==(const Pos& other) const { return x == other.x && y == other.y; }

    struct Hash {
        size_t operator()(const Pos& self) const {
            return (std::hash<int>()(self.x) << 1) ^ (std::hash<int>()(self.y));
        }
    };

private:
    int x, y;
};


main {
    std::string regex;
    std::getline(std::cin, regex);

    // This is a unique representation of the graph, given we know it's grid aligned and bidirectional
    // An edge exists between (x, y) and (x + 1, y) if (x, y, Axis::X) is present in the edge set
    // An edge exists between (x, y) and (x, y + 1) if (x, y, Axis::Y) is present in the edge set
    std::unordered_set<Edge, Edge::Hash> edges;
    std::vector<Pos> stack ({ Pos(0, 0) });
    Pos start, pos;

    for (const char& c : regex) {
        switch (c) {
            case '$':
            case '^':
                break; // Special regex characters, don't care
            case 'N':
            case 'E':
            case 'S':
            case 'W':
                edges.insert(pos.edge(c));
                pos = pos.adj(c);
                break;
            case '(':
                stack.push_back(start);
                start = pos;
                break;
            case '|':
                pos = start;
                break;
            case ')':
                start = stack.back();
                stack.pop_back();
                break;
            default: unreachable;
        }
    }

    // Next, we need to search this graph to find all paths of all lengths
    // This is a standard BFS through a graph with cardinal direction adjacency, just different due to the need to check each edge in the edge set
    std::deque<std::pair<Pos, int>> queue ({ std::pair(Pos(0, 0), 0) });
    std::unordered_map<Pos, int, Pos::Hash> paths;
    std::string directions = "NEWS";

    while (queue.size() > 0) {
        auto& [pos, dist] = queue.front();        
        paths[pos] = dist;
        
        for (const char& c : directions) {
            const Edge edge = pos.edge(c);
            if (edges.find(edge) != edges.end()) {
                const Pos next = pos.adj(c);
                if (paths.find(next) == paths.end()) {
                    queue.push_back(std::pair(next, dist + 1));
                }
            }
        }

        queue.pop_front();
    }

    // Part 1 is the max distance of all paths we find
    // Part 2 is the number of paths with distance >= 1k
    int part1 = 0, part2 = 0;
    for (auto& path : paths) {
        if (path.second > part1) part1 = path.second;
        if (path.second >= 1000) part2++;
    }

    println("Part 1: %d", part1);
    println("Part 2: %d", part2);
}