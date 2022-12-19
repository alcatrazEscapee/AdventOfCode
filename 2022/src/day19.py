# Day 19: Not Enough Minerals
# Rank: 269 / 141

import functools

from utils import get_input, ints

def main(text: str):
    lines = text.split('\n')
    bps = []
    for line in lines:
        bp_id, ore_r_ore_cost, clay_r_ore_cost, obsid_r_ore_cost, obsid_r_clay_cost, geo_r_ore_cost, geo_r_obsid_cost = ints(line)
        bps.append((bp_id, (ore_r_ore_cost, 0), (clay_r_ore_cost, 0), (obsid_r_ore_cost, obsid_r_clay_cost), (geo_r_ore_cost, geo_r_obsid_cost)))

    print('bps', bps)
    ans = 0
    for bp_id, bp in enumerate(bps):
        print(bpc := max_geos(bp, 0, 0, 0, 0, 0, 1, 0, 0, 0), bpc * (bp_id + 1), (bp_id + 1))
        ans += bpc * (bp_id + 1)
    print('FNAL', ans)


@functools.lru_cache(None)
def max_geos(bp, t, ore, clay, obsid, geos, ore_rs, clay_rs, obsid_rs, geos_rs):
    if t == 32:
        return geos

    bp_id, (ore_r_ore_cost, _), (clay_r_ore_cost, _), (obsid_r_ore_cost, obsid_r_clay_cost), (geo_r_ore_cost, geo_r_obsid_cost) = bp

    b_ore = ore
    b_clay = clay
    b_obsid = obsid
    b_geos = geos

    #print(t, ore, clay, obsid, geos, ore_rs, clay_rs, obsid_rs, geos_rs)

    ore += ore_rs
    clay += clay_rs
    obsid += obsid_rs
    geos += geos_rs

    best_so_far = 0
    buy = False
    if b_ore >= geo_r_ore_cost and b_obsid >= geo_r_obsid_cost:
        buy = True
        best_so_far = max(best_so_far, max_geos(bp, t + 1, ore - geo_r_ore_cost, clay, obsid - geo_r_obsid_cost, geos, ore_rs, clay_rs, obsid_rs, geos_rs + 1))
    else:
        if b_ore >= obsid_r_ore_cost and b_clay >= obsid_r_clay_cost:
            buy = True
            best_so_far = max(best_so_far, max_geos(bp, t + 1, ore - obsid_r_ore_cost, clay - obsid_r_clay_cost, obsid, geos, ore_rs, clay_rs, obsid_rs + 1, geos_rs))
        else:
            if b_ore >= ore_r_ore_cost:
                buy = True
                best_so_far = max(best_so_far, max_geos(bp, t + 1, ore - ore_r_ore_cost, clay, obsid, geos, ore_rs + 1, clay_rs, obsid_rs, geos_rs))
            if b_ore >= clay_r_ore_cost:
                buy = True
                best_so_far = max(best_so_far, max_geos(bp, t + 1, ore - clay_r_ore_cost, clay, obsid, geos, ore_rs, clay_rs + 1, obsid_rs, geos_rs))

            # Don't buy anything
            best_so_far = max(best_so_far, max_geos(bp, t + 1, ore, clay, obsid, geos, ore_rs, clay_rs, obsid_rs, geos_rs))
    return best_so_far

if __name__ == '__main__':
    main(get_input(19))
