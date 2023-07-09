# Day 19: Not Enough Minerals
# Rank: 269 / 141

from utils import get_input, ints
from typing import NamedTuple, List
from time import time_ns

import math
import functools


class Blueprint(NamedTuple):
    id: int
    ore_robot_ore_cost: int
    clay_robot_ore_cost: int
    obsidian_robot_ore_cost: int
    obsidian_robot_clay_cost: int
    geode_robot_ore_cost: int
    geode_robot_obsidian_cost: int


def main(text: str):
    blueprints: List[Blueprint] = [Blueprint(*ints(line)) for line in text.split('\n')]

    print('Part 1:', sum(solve(blueprint, 24) * blueprint.id for blueprint in blueprints))
    print('Part 2:', math.prod(solve(blueprint, 32) for blueprint in blueprints[:3]))


def solve(blueprint: Blueprint, max_time: int) -> int:

    # Reduce the state space!
    # 1. We can only produce one robot per minute, so we never need more of a given type of resource than can be consumed in a given minute.
    #    After that point, we don't even need to care about how much of that resource we have, or how many of the robots we have!
    # 2. If we don't purchase a robot in one round that we could've, then we cannot purchase that robot in a future round (as we should've bought it now)
    #    This affects the 'don't buy anything' case, which we then pass robots that we can't buy
    # 3. If we ever reach a point where the time left * the maximum amount of a given resource we could spend per turn on a given resource is less than
    #    our supply of that resource, we don't allow buying any more of that robot - even if we aren't producing enough to exceed our buy rate.
    # 4. We can stop at max_time - 1 and return geodes + geodes_robots, rather than doing an additional iteration choosing what to buy
    # 5. We can instead stop at max_time - 2 and return geodes + 2 * geodes_robots + (if we can afford a geode robot, 1 else 0), rather than spending an additional iteration!

    max_ore = max(blueprint.ore_robot_ore_cost, blueprint.clay_robot_ore_cost, blueprint.obsidian_robot_ore_cost, blueprint.geode_robot_ore_cost)
    max_clay = blueprint.obsidian_robot_clay_cost
    max_obsidian = blueprint.geode_robot_obsidian_cost

    max_consumable_ore = max_time * max_ore
    max_consumable_clay = max_time * max_ore
    max_consumable_obsidian = max_time * max_ore

    print('Blueprint %d State Space = %d Minutes x Robots (%d x %d x %d) x Resources (%d x %d x %d) = %d' % (
        blueprint.id,
        max_time,
        max_ore, max_clay, max_obsidian,
        max_consumable_ore, max_consumable_clay, max_consumable_obsidian,
        state_space := max_time * max_ore * max_clay * max_obsidian * max_consumable_ore * max_consumable_clay * max_consumable_obsidian))

    @functools.lru_cache(None)
    def recurse(time: int, ore: int, clay: int, obsidian: int, geodes: int, ore_robots: int, clay_robots: int, obsidian_robots: int, geode_robots: int, allow_ore: bool = True, allow_clay: bool = True, allow_obsidian: bool = True, allow_geode: bool = True) -> int:

        afford_geode = ore >= blueprint.geode_robot_ore_cost and obsidian >= blueprint.geode_robot_obsidian_cost

        if time == max_time - 2:  # Terminal condition - skips the last two iterations because the result is easy to compute and saves iterations
            return geodes + 2 * geode_robots + (1 if afford_geode else 0)

        # Allowable conditions: these implement notes (1) and (3) from above
        if (ore_robots >= max_ore and ore >= max_ore) or ore - max_ore >= (max_ore - ore_robots) * (max_time - time):
            ore_robots = ore = max_ore
            allow_ore = False

        if (clay_robots >= max_clay and clay >= max_clay) or clay - max_clay >= (max_clay - clay_robots) * (max_time - time):
            clay_robots = clay = max_clay
            allow_clay = False

        if (obsidian_robots >= max_obsidian and obsidian >= max_obsidian) or obsidian - max_obsidian >= (max_obsidian - obsidian_robots) * (max_time - time):
            obsidian_robots = obsidian = max_obsidian
            allow_obsidian = False

        afford_ore = ore >= blueprint.ore_robot_ore_cost
        afford_clay = ore >= blueprint.clay_robot_ore_cost
        afford_obsidian = ore >= blueprint.obsidian_robot_ore_cost and clay >= blueprint.obsidian_robot_clay_cost

        value = 0
        if afford_ore and allow_ore:
            value = max(value, recurse(time + 1, ore + ore_robots - blueprint.ore_robot_ore_cost, clay + clay_robots, obsidian + obsidian_robots, geodes + geode_robots, ore_robots + 1, clay_robots, obsidian_robots, geode_robots))
        if afford_clay and allow_clay:
            value = max(value, recurse(time + 1, ore + ore_robots - blueprint.clay_robot_ore_cost, clay + clay_robots, obsidian + obsidian_robots, geodes + geode_robots, ore_robots, clay_robots + 1, obsidian_robots, geode_robots))
        if afford_obsidian and allow_obsidian:
            value = max(value, recurse(time + 1, ore + ore_robots - blueprint.obsidian_robot_ore_cost, clay + clay_robots - blueprint.obsidian_robot_clay_cost, obsidian + obsidian_robots, geodes + geode_robots, ore_robots, clay_robots, obsidian_robots + 1, geode_robots))
        if afford_geode and allow_geode:
            value = max(value, recurse(time + 1, ore + ore_robots - blueprint.geode_robot_ore_cost, clay + clay_robots, obsidian + obsidian_robots - blueprint.geode_robot_obsidian_cost, geodes + geode_robots, ore_robots, clay_robots, obsidian_robots, geode_robots + 1))

        # Consider not buying anything
        # Only allow buying things in the future we cannot afford now - this implements note (2) from above.
        value = max(value, recurse(time + 1, ore + ore_robots, clay + clay_robots, obsidian + obsidian_robots, geodes + geode_robots, ore_robots, clay_robots, obsidian_robots, geode_robots, not afford_ore, not afford_clay, not afford_obsidian, not afford_geode))

        return value

    start = time_ns()
    max_value = recurse(0, 0, 0, 0, 0, 1, 0, 0, 0)
    stop = time_ns()

    info = recurse.cache_info()
    print('Took %d ms, ' % ((stop - start) // 1_000_000), end='')
    print('state space explored = %d (%2.6f%%), cache hit = %2.1f%%, ' % (info.currsize, 100 * info.currsize / state_space, 100 * info.hits / (info.hits + info.misses)), end='')
    print('result = %d' % max_value)
    return max_value


if __name__ == '__main__':
    main(get_input(19))
