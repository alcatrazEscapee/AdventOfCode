from utils import get_input
from math import lcm


def main(text: str):
    lines = text.split('\n')


    #vals = [719, 1106 - 719, 2811 - 1106, 2860 - 2811]
    #print(sum(vals) * 250000000000)


    ROCKS = [
        {(0, 0), (1, 0), (2, 0), (3, 0)},  # line
        {(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)},  # plus
        {(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)},  # L backwards
        {(0, 0), (0, 1), (0, 2), (0, 3)},
        {(0, 0), (0, 1), (1, 0), (1, 1)}
    ]

    last = 0

    MOVES = text
    #MOVES = '>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>'

    inverval = lcm(len(MOVES), len(ROCKS))

    seen = set()
    seen2 = set()

    time = 1000000000000 - 120
    p1 = False

    start_cyc = 0

    jet_tick = 0
    rock_tick = 0
    floor = {(x, 0) for x in range(7)}
    num_rocks = 0
    last_delta = 0
    last_num_rocks = 0
    bonus_y = 0
    p2 = False
    while True:

        if num_rocks % 100 == 0:
            print(num_rocks)
        max_y_in_floor = max(y for _, y in floor)
        #print(max_y_in_floor)

        top_floor = frozenset((x, y - max_y_in_floor) for x, y in floor if y >= max_y_in_floor - 70)



        st = ((top_floor, rock_tick, jet_tick))

        if st in seen2 and not p2:
            print('recurse again at', num_rocks)
            old_n_rocks, old_y = start_cyc
            print('from', *start_cyc, 'to', num_rocks, max_y_in_floor)
            print('cycle len', cyc_len := (num_rocks - old_n_rocks))
            print(repeats := (1000000000000 - num_rocks) // cyc_len)
            num_rocks += repeats * cyc_len
            bonus_y = (max_y_in_floor - old_y) * repeats
            p2 = True
            print('bonus y', bonus_y, 'and now', num_rocks)

        if st in seen:
            if not p1:
                print('recurse! at', num_rocks)
                start_cyc = num_rocks, max_y_in_floor
            p1 = True
            seen2.add(st)
        seen.add(st)

        floor = {(x, y) for x, y in floor if y > max_y_in_floor - 100}

        if num_rocks % inverval == 0:
            print(num_rocks, num_rocks - last_num_rocks, max_y_in_floor, max_y_in_floor - last)
            last = max_y_in_floor
            last_num_rocks = num_rocks

        #if sum((x, max_y_in_floor) in floor for x in range(7)) == 6:
            #yield jet_tick, rock_tick
            #print('yes at', num_rocks)
            #print('max y', max_y_in_floor)

        rock = {(x + 2, max_y_in_floor + y + 4) for x, y in ROCKS[rock_tick]}
        rock_tick = (rock_tick + 1) % len(ROCKS)

        while True:
            jet = MOVES[jet_tick]
            jet_tick = (jet_tick + 1) % len(MOVES)

            #print(rock)

            #print('===')
            #print(InfiniteGrid.of_points({(x, -y) for x, y in floor | rock}))
            #print('===')


            # BLOCKED 0 1 2 3 4 5 6 BLOCKED

            if jet == '<':
                test_rock = {(x - 1, y) for x, y in rock}
                if any(r in floor for r in test_rock) or any(x < 0 for x, _ in test_rock):  # blocked
                    pass
                else:
                    rock = test_rock
            elif jet == '>':
                test_rock = {(x + 1, y) for x, y in rock}
                if any(r in floor for r in test_rock) or any(x > 6 for x, _ in test_rock):  # blocked
                    pass
                else:
                    rock = test_rock

            # move down
            test_rock = {(x, y - 1) for x, y in rock}
            if any(r in floor for r in test_rock):
                # stop
                #print('is down')
                floor |= rock
                num_rocks += 1
                #yield jet_tick, rock_tick, max(y for _, y in floor)
                if num_rocks == 1000000000000:
                    print(max(y for _, y in floor) + bonus_y)
                    return
                #jet_tick = (jet_tick + 1) % len(MOVES)
                break  # new rock
            else:
                rock = test_rock
        #if num_rocks > 4:
        #    return



if __name__ == '__main__':
    main(get_input(17))
