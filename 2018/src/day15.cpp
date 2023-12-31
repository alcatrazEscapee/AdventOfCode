#include "aoc.h"
#include "../lib/point.h"


// Points compare based on their reading order, which is used for selecting targets, moving, etc.
auto operator<=>(const Point& left, const Point& right) {
    if (auto cmp = left.y <=> right.y; cmp != 0) return cmp;
    return left.x <=> right.x;
}

// Points for the four adjacent cardinal directions. Immutable.
static const Point CARDINALS[4] = { Point(0, -1), Point(-1, 0), Point(1, 0), Point(0, 1) };


/// A path used during path finding.
/// pos represents the current point, start represents the initial step (needed to determine which direction to move)
class Path {
public:

    Path() = default;
    Path(Point pos, Point start, int dist) : pos(pos), start(start), dist(dist) {}

    // The best path is sorted first by distance, then by order of the position
    auto operator<=>(const Path& other) const {
        if (auto cmp = this->dist <=> other.dist; cmp != 0) return cmp;
        return this->pos <=> other.pos;
    }

    Point pos;
    Point start;
    int dist;
};

/// @brief An entity or unit - either an elf or a goblin.
/// A unit has an HP, attack value, and position, plus utilities to check if it is a given type, alive, or dead.
class Entity {
public:

    enum class Type { Elf, Goblin };

    Entity(int x, int y, Type type, int atk, int hp) :
        pos(Point(x, y)),
        type(type),
        atk(atk),
        hp(hp) {} 
    
    bool is_dead() const { return this->hp <= 0; }
    bool is_alive() const { return !this->is_dead(); }

    bool is_enemy_of(const Entity& other) const { return this->type != other.type; }

    Point pos;
    Type type;
    int atk;
    int hp;
};


/// @brief The class used to manage and simulate combat
class Combat {
public:

    Combat(std::vector<std::string> grid, int elf_atk, int goblin_atk, int all_hp) : grid(grid) {
        const int width = grid.size(),
                  height = grid.at(0).length();
        
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                char c = this->grid.at(y).at(x);
                if (c == 'G' || c == 'E') {
                    this->grid.at(y).at(x) = '.';
                    this->entities.push_back(Entity(x, y, c == 'G' ? Entity::Type::Goblin : Entity::Type::Elf, c == 'G' ? goblin_atk : elf_atk, all_hp));
                }
            }
        }
    }

    /// @brief Simulates combat until either group has won.
    /// @param no_deaths If true, then combat will be aborted and -1 returned as soon as any elf has died.
    /// @return -1 if no_deaths, and an elf died, otherwise the answer for this combat (total HP remaining * number of rounds)
    int do_combat(bool no_deaths) {
        for (int rounds = 0;; rounds++) {
            this->sort(); // Entities take actions in reading order
            for (Entity& entity : this->entities) {

                if (entity.is_dead()) continue; // Dead entities don't take a turn
                if (!this->has_enemies(entity)) { // If at the start of this entities turn, we don't have any enemies, combat is over
                    return rounds * this->total_hp();
                }

                this->do_navigate(entity); // Move this entity, if desired
                this->do_attack(entity); // Then attack
            }

            if (no_deaths && this->any_elves_have_died()) {
                return -1; // If we don't permit any elves to die, return -1
            }
        }
    }

private:

    /// @brief Handles moving a given entity, and finding the correct path to do so with.
    void do_navigate(Entity& entity) {
        // If there is any enemy within range, then don't move
        for (const Point& adj : CARDINALS)
            if (this->enemy_at(entity, entity.pos + adj) != nullptr)
                return;
        
        // Calculate all possible target positions
        // This consists of every point that we can navigate to, i.e. is empty, and adjacent to a known enemy
        std::unordered_set<Point, Point::Hash> targets;
        for (const Entity& enemy : this->entities) {
            if (enemy.is_alive() && enemy.is_enemy_of(entity)) {
                for (const Point& adj : CARDINALS) {
                    const Point pos = enemy.pos + adj;
                    if (this->is_free(pos)) {
                        targets.insert(pos);
                    }
                }
            }
        }

        // If the entity cannot identify any open target positions, then it ends it's turn
        if (targets.size() == 0) {
            return;
        }

        // Path finding uses Dikjkstra's algorithm to find the best path to the enemy
        // We need this, not only to choose which enemy to move towards, but how to move towards it as well
        std::unordered_map<Point, Path, Point::Hash> paths;
        std::deque<Path> queue;

        // First four points determine the starting position for each path
        // As we explore, we track the best starting position to each point
        for (const Point& adj : CARDINALS) {
            const Point pos = entity.pos + adj;
            if (this->is_free(pos)) {
                const Path path = Path(pos, adj, 1);
                queue.push_back(path);
                paths[pos] = path;
            }
        }

        // This is a minor optimization to repeated path finding
        // Since we always choose a target that is the closest, we can stop exploring once we are only identifying targets beyond that distance.
        // The nature of our BFS means we have a monotonically increasing queue of distances
        int chosen_dist = -1;

        while (queue.size() > 0) {
            const Path path = queue.front();
            queue.pop_front();

            // Check if we have found any target, at this point
            // If we have, we can set the minimum distance, if unset
            if (targets.contains(path.pos) && chosen_dist == -1) {
                chosen_dist = path.dist;
            }

            // Then, if we ever find paths > the chosen distance, we can abort the BFS
            if (chosen_dist != -1 && path.dist > chosen_dist) {
                break;
            }

            // Consider adjacent moves
            for (const Point& adj : CARDINALS) {
                const Point pos = path.pos + adj;
                if (this->is_free(pos)) { // The target location is free to move to
                    const Path next = Path(pos, path.start, path.dist + 1);
                    auto& prev = paths[pos];
                    if (prev.dist <= 0 || next < prev) { // If this is the first time we find the path, or we find a shorter path
                        prev = next;
                        queue.push_back(next); // Enqueue the path for exploration
                    }
                }
            }
        }

        // Iterate all target positions and choose the best one, and move in that direction according to the path map
        const Path* chosen = nullptr;
        for (const Point& target : targets) {
            const Path& path = paths[target];
            if (
                path.dist > 0 && ( // The path must be a real path (not the origin, or unreachable), then;
                chosen == nullptr || // The current chosen path must be null, or;
                path.dist < chosen->dist || // This path must be shorter than the chosen path, or;
                (path.dist == chosen->dist && path.pos < chosen->pos) // This path must be the same distance, but lower ordinal
            )) {
                chosen = &path;
            }
        }

        // Finally, move the entity by the chosen path
        if (chosen != nullptr) {
            entity.pos += chosen->start;
        }
    }

    /// @brief Handles attacking any adjacent enemies, if possible.
    void do_attack(Entity& entity) {
        // Identify the chosen enemy to attack
        // All adjacent enemies are checked, and the chosen one is prioritized by HP (min), then by sort order.
        Entity* chosen = nullptr;
        for (const Point& adj : CARDINALS) {
            Entity* enemy = this->enemy_at(entity, entity.pos + adj);
            if (enemy != nullptr) {
                if (
                    chosen == nullptr || // No chosen enemy yet
                    enemy->hp < chosen->hp || // A enemy with a lower HP is found
                    (enemy->hp == chosen->hp && enemy->pos < chosen->pos) // An enemy with the same HP, but better sort order is found
                ) {
                    chosen = enemy;
                }
            }
        }

        if (chosen != nullptr) {
            chosen->hp -= entity.atk;
        }
    }

    /// @brief Sort all entities by their priority, before the round begins.
    void sort() {
        std::sort(
            this->entities.begin(), this->entities.end(),
            [](const Entity& a, const Entity& b) { return a.pos < b.pos; });
    }

    /// @return The total HP of all alive entities. 
    int total_hp() const {
        int total_hp = 0;
        for (const Entity& e : this->entities)
            if (e.is_alive())
                total_hp += e.hp;
        return total_hp;
    }

    /// @return true if at least one alive enemy of this entity exists.
    bool has_enemies(const Entity& entity) const {
        for (const Entity& other : this->entities)
            if (other.is_alive() && other.is_enemy_of(entity))
                return true;
        return false;
    }

    /// @return true if any elves are currently dead.
    bool any_elves_have_died() const {
        for (const Entity& e : this->entities)
            if (e.is_dead() && e.type == Entity::Type::Elf)
                return true;
        return false;
    }

    /// @return The entity at pos, if there is one, which is both alive and an enemy of the provided entity.
    Entity* enemy_at(const Entity& entity, const Point& pos) {
        for (Entity& other : this->entities)
            if (other.pos == pos && other.is_alive() && other.is_enemy_of(entity))
                return &other;
        return nullptr;
    }

    /// @return true if the given position is free of obstructions (walls, or alive entities) 
    bool is_free(const Point& pos) const {
        if (this->is_wall(pos)) return false;
        for (const Entity& entity : this->entities)
            if (entity.is_alive() && entity.pos == pos)
                return false;
        return true;
    }

    /// @return true if the given position is a wall
    bool is_wall(const Point& pos) const { return this->grid[pos.y][pos.x] == '#'; }


    std::vector<std::string> grid;
    std::vector<Entity> entities;
};


main {
    std::vector<std::string> grid;
    std::string line;
    while (std::getline(std::cin, line)) {
        grid.push_back(line);
    }
    
    Combat* map;
    bool part1 = false;
    int ret;

    for (int atk = 3;; atk++) {
        map = new Combat(grid, atk, 3, 200);
        ret = map->do_combat(part1);

        if (!part1) {
            part1 = true;
            println("Part 1: %d", ret);
        } else if (ret != -1) {
            println("Part 2: %d", ret);
            break;
        }

        delete map;
    }
}