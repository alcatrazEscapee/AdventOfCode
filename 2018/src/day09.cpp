#include "aoc.h"

typedef unsigned long long int u64;

u64 play(int max_turns, int max_players);

void rotate(std::deque<int>& deque, int n);

main {
    assert(play(25, 9) == 32, "Test 1");
    assert(play(1618, 10) == 8317, "Test 2");
    assert(play(7999, 13) == 146373, "Test 3");
    assert(play(1104, 17) == 2764, "Test 4");
    assert(play(6111, 21) == 54718, "Test 5");
    assert(play(5807, 30) == 37305, "Test 6");

    int max_turns, max_players;

    scanf("%d players; last marble is worth %d points\n", &max_players, &max_turns);
    println("Part 1: %llu", play(max_turns, max_players));
    println("Part 2: %llu", play(max_turns * 100, max_players));
}

u64 play(int max_turns, int max_players) {
    std::unordered_map<int, u64> players;
    std::deque<int> marbles ({ 1 });

    for (int turn = 1; turn <= max_turns; turn++) {
        if (turn % 23 == 0) {
            rotate(marbles, 6);
            players[turn % max_players] += turn + marbles.back();
            marbles.pop_back();
        } else {
            rotate(marbles, -2);
            marbles.push_front(turn);
        }
    }

    return std::max_element(
        players.begin(), players.end(),
        [](const std::pair<int, u64>& a, const std::pair<int, u64>& b) { return a.second < b.second; })
        ->second;
}

void rotate(std::deque<int>& deque, int n) {
    if (n > 0) {
        for (int i = 0; i < n; i++) {
            int value = deque.back();
            deque.pop_back();
            deque.push_front(value);
        }
    } else {
        for (int i = 0; i < -n; i++) {
            int value = deque.front();
            deque.pop_front();
            deque.push_back(value);
        }
    }
}