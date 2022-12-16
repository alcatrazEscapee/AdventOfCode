# Day 16: Rank 24 / 431

from utils import get_input, ints, a_star
from collections import defaultdict

import heapq
import functools


def main(text: str):
    lines = text.split('\n')

    graph = {}
    for line in lines:
        _, valve, _, _, rate, _, _, _, _, *valves = line.split(' ')
        rate = ints(rate)[0]
        valves = [v.replace(',', '') for v in valves]
        graph[valve] = (rate, valves)

    good_valves = []
    for k, (v, adj) in graph.items():
        if v > 0:
            good_valves.append(k)

    def graph_step(p):
        (_, ps) = graph[p]
        for q in ps:
            yield q

    start = 'AA'
    good_graph = defaultdict(list)
    for good_valve in good_valves:
        for other in good_valves:
            if good_valve != other:
                cost = a_star(good_valve, other, graph_step).cost
                good_graph[good_valve].append((other, cost))
        cost = a_star('AA', good_valve, graph_step).cost
        good_graph[start].append((good_valve, cost))


    for g, paths in good_graph.items():
        print(g, paths)

    TIME = 26
    all_good = len(good_graph)

    @functools.lru_cache(None)
    def solve(ignored_set):
        queue = [(0, start, ignored_set, 0, 0)]  # time, pos, opened, flow, rate
        best = 0
        while queue:
            time, last, opened, flow, rate = heapq.heappop(queue)
            for adj, step_time in good_graph[last]:
                if adj not in opened:
                    adj_time = time + step_time
                    if adj_time < TIME:
                        new_opened = set(opened)
                        new_opened.add(adj)

                        add_rate = graph[adj][0]

                        heapq.heappush(queue, (adj_time + 1, adj, new_opened, flow + rate * (step_time + 1), rate + add_rate))
                    else:
                        rem_time = TIME - time
                        total_flow = flow + rate * (rem_time)
                        if total_flow > best:
                            best = total_flow

            if len(opened) == all_good - 1:
                rem_time = TIME - time
                total_flow = flow + rate * (rem_time)
                if total_flow > best:
                    best = total_flow
        return best

    #print(solve(frozenset(['NQ', 'QW'])))

    best_values = 0
    all_values = set(good_valves)

    print(good_valves)
    for i in range(1 << len(good_valves)):
        ignore = set()
        for j in range(len(good_valves)):
            if (i >> j) & 1 == 1:
                ignore.add(good_valves[j])

        you = solve(frozenset(ignore))
        them = solve(frozenset(all_values - ignore))
        total = you + them

        if total > best_values:
            best_values = total
        print('i =', i, 'new best with', best_values, 'total', total, '=', you, '+', them, 'slice', ignore)


if __name__ == '__main__':
    main(get_input(16))
