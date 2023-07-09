# Day 16: Proboscidea Volcanium
# Rank 24 / 431

from utils import get_input
from time import time_ns
from collections import defaultdict
from typing import Dict, Tuple, List

import re
import sys
import functools


def main(text: str):
    flows, graph = parse(text)
    pairs = floyd_warshall(graph)
    flows, graph = build_graph(flows, pairs)
    solve(flows, graph)


def parse(text: str) -> Tuple[Dict[str, int], Dict[str, List[str]]]:
    # Parses the input into a map of vertex -> flow, and vertex -> [vertices]
    graph: Dict[str, List[str]] = {}
    flows: Dict[str, int] = {}

    for line in text.split('\n'):
        valve, rate, valves = re.search(r'Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z, ]+)', line).groups()
        graph[valve] = valves.split(', ')
        flows[valve] = int(rate)
    return flows, graph

def floyd_warshall(graph: Dict[str, List[str]]) -> Dict[Tuple[str, str], int]:
    # Implements Floyd-Warshall Algorithm (which is a nice O(|V|^3))
    # Returns a graph of the pairwise distances between any two edges
    pairs = defaultdict(lambda: sys.maxsize)
    for p, qs in graph.items():
        pairs[p, p] = 0
        for q in qs:
            pairs[q, p] = pairs[p, q] = 1
    for r in graph.keys():
        for p in graph.keys():
            for q in graph.keys():
                pairs[p, q] = min(pairs[p, q], pairs[p, r] + pairs[r, q])
    return pairs

def build_graph(flows: Dict[str, int], graph: Dict[Tuple[str, str], int]) -> Tuple[List[int], List[List[Tuple[int, int]]]]:
    # Performs some additional sanitizations on the input graphs
    # 1. Restructures the graph of edges -> weights to a map of vertex -> (vertex, weight), for fast iteration.
    # 2. Drops any edges which are between flow = 0 nodes, unless those are edges from 'AA' -> a flow > 0 node.
    # 3. Removes all self edges present in the graph
    # 4. Adds +1 to all edge weights, to represent the time taken to open the valve
    # 5. Map all graph nodes to integers, so we can use a much more efficient 'seen' graph that can copy faster (a bitmask)
    pruned = defaultdict(list)
    for (p, q), d in graph.items():
        if (flows[p] > 0 or p == 'AA') and flows[q] > 0 and p != q:
            pruned[p].append((q, d + 1))
    flows = {v: f for v, f in flows.items() if f > 0}
    keys = ['AA'] + list(flows.keys())
    flows = [flows[v] if v in flows else 0 for v in keys]
    pruned = [[(keys.index(vi), vn) for vi, vn in pruned[k]] if k in pruned else [] for k in keys]
    return flows, pruned

def solve(flows: List[int], graph: List[List[Tuple[int, int]]], max_time: int = 30):
    state_space = 2**len(flows) * len(flows) * max_time
    start = time_ns()
    print('Running... size of state space is 2^%d x %d x %d = %d' % (len(flows), len(flows), max_time, state_space))

    def print_info():  # Print some interesting info after each iteration
        info = calculate_flow_recursive.cache_info()
        print('Took %d ms, ' % ((time_ns() - start) // 1_000_000), end='')
        print('state space explored = %2.1f%%, cache hit = %2.1f%%' % (100 * info.currsize / state_space, 100 * info.hits / (info.hits + info.misses)))

    visited_all = (1 << len(flows)) - 1

    @functools.lru_cache(None)
    def calculate_flow_recursive(visited: int, pos: int, time: int) -> int:
        # Returns the maximum flow you can achieve
        # State is represented as {visited} x position x time
        if visited == visited_all:
            return 0
        if time >= max_time:
            return 0
        best_flow = 0
        for next_pos, step_time in graph[pos]:
            if (visited >> next_pos) & 1 == 0:
                next_visited = visited | (1 << next_pos)
                next_time = time + step_time

                if next_time < max_time:
                    # If we reach the next position in time, we add the flow that this node will accumulate over the entire lifetime
                    # This is the rate of flow for this node * the amount of time remaining before the max time
                    flow = flows[next_pos] * (max_time - next_time) + calculate_flow_recursive(next_visited, next_pos, next_time)
                    if flow > best_flow:
                        best_flow = flow
        return best_flow

    # Part 1, we start at 'AA' with no flow, and no nodes visited
    print('Part 1:', calculate_flow_recursive(0, 0, 0))
    print_info()
    start = time_ns()

    # Part 2, we need to track two different movements
    # We do this by splitting the set of possible nodes to visit, as both movements should be independent
    # We can re-use the recursive function by faking two things:
    # 1. we start with a set of nodes already visited - this will then only consider nodes not in that set as possible.
    # 2. We start with a time of 4, meaning we'll still do the same max_time comparisons.
    # Both of those two facts *should* cause the memoization on our recursive function to get us through the next part in a reasonable time
    # In the worst case, our state space only gets bigger by a factor of 1 << len(flows)
    # Note that we can always consider state '0' visited, since it is our starting state and we never return to it
    part2 = 0
    for i in range(1 << (len(flows))):
        left = calculate_flow_recursive(i | 1, 0, 4)
        right = calculate_flow_recursive(((~i) & visited_all) | 1, 0, 4)
        total = left + right

        if total > part2:
            print('Iteration %d / %d (%2.1f%%): Found new max of %d = %d + %d' % (i, 1 << len(flows), 100 * i / (1 << len(flows)), total, left, right))
            part2 = total

    print('Part 2:', part2)
    print_info()


if __name__ == '__main__':
    main(get_input(16))
