#include "aoc.h"

#define SIZE 1000

itype(unsigned int, 4) u32;

class Bot {
public:
    bool in_range(int px, int py, int pz) const {
        return std::abs(px - x) + std::abs(py - y) + std::abs(pz - z) <= r;
    }

    int x, y, z, r;
};

/// @brief A cube representing the product of [x, x + w) x [y, y + w) x [z, z + w) in 3D
class Cube {
public:
    Cube(const Bot* bots, int x, int y, int z, int w) : x(x), y(y), z(z), w(w) {
        // Count the number of bots within range of this cube
        for (int i = 0; i < SIZE; i++) {
            const Bot* bot = &bots[i];

            // Project the bot onto a face of the cube, or inside the cube.
            // Then check if the bot is in range of that point.
            int botX = project(bot->x, x),
                botY = project(bot->y, y),
                botZ = project(bot->z, z);
            
            if (bot->in_range(botX, botY, botZ)) {
                score++;
            }
        }
    }

    int distance() const {
        return std::abs(x) + std::abs(y) + std::abs(z);
    }

private:
    int project(int bot, int coord) const {
        return bot < coord ? coord : (bot >= coord + w ? coord + w - 1 : bot);
    }

public:
    int x, y, z;
    int w;
    int score;
};


main {
    Bot* bots = new Bot[SIZE];
    Bot* max_radius = nullptr;
    u32 max_coordinate = 0;
    for (int i = 0; i < SIZE; i++) {
        Bot* bot = &bots[i];
        assert(scanf("pos=<%d,%d,%d>, r=%d\n", &bot->x, &bot->y, &bot->z, &bot->r) == 4, "reading input line %d", i);

        if (max_radius == nullptr || bot->r > max_radius->r) {
            max_radius = bot;
        }

        max_coordinate = 
            std::max((u32) std::abs(bot->x),
            std::max((u32) std::abs(bot->y),
            std::max((u32) std::abs(bot->z), max_coordinate)));
    }

    // Part 1: Find the # of bots in range of the max radius bot
    int count = 0;
    for (int i = 0; i < SIZE; i++) {
        Bot* bot = &bots[i];
        if (max_radius->in_range(bot->x, bot->y, bot->z)) {
            count++;
        }
    }

    println("Part 1: %d", count);

    // Part 2: Find a single point that is within range of the most bots possible
    // Use an octree search here. This is not guaranteed to be the most efficient, or time complex solution,
    // and can and will iterate over every single point in the space, when ran on pathological inputs. But the problem
    // is designed so this works, so let's give it a try.

    // First, compute a cube radius ensured to cover everything
    u32 radius = std::bit_ceil(max_coordinate);
    
    // We need a priority for our priority queue
    // Sort by # of intersections, then by size, then by (shortest) distance to origin
    struct Compare {
        bool operator()(const Cube& a, const Cube& b) {
            if (a.score != b.score) return a.score < b.score;
            if (a.w != b.w) return a.w < b.w;
            return a.distance() > b.distance();
        }
    };

    std::priority_queue<Cube, std::vector<Cube>, Compare> queue;
    Cube origin = Cube(bots, -radius, -radius, -radius, radius << 1);

    queue.push(origin);

    while (queue.size() != 0) {
        Cube cube = queue.top();
        queue.pop();

        if (cube.w == 1) {
            println("Part 2: %d", cube.distance());
            break;
        }

        // Split the cube into eight sub-cubes, then requeue
        int w = cube.w >> 1;
        queue.push(Cube(bots, cube.x,     cube.y,     cube.z,     w));
        queue.push(Cube(bots, cube.x + w, cube.y,     cube.z,     w));
        queue.push(Cube(bots, cube.x,     cube.y + w, cube.z,     w));
        queue.push(Cube(bots, cube.x,     cube.y,     cube.z + w, w));
        queue.push(Cube(bots, cube.x + w, cube.y + w, cube.z,     w));
        queue.push(Cube(bots, cube.x + w, cube.y,     cube.z + w, w));
        queue.push(Cube(bots, cube.x,     cube.y + w, cube.z + w, w));
        queue.push(Cube(bots, cube.x + w, cube.y + w, cube.z + w, w));
    }
}