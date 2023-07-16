# Day 11: Space Police

from utils import *
from collections import defaultdict


def main(values, part2: bool):
    panels = defaultdict(int)  # 0 = black, 1 = white
    painted_once = set()
    code = IntCode(values, [])

    robot = (0, 0)
    robot_dir = (0, 1)  # UP

    # For part 2, the robot starts on a white tile
    if part2:
        panels[robot] = 1

    while code.running:
        code.inputs.append(panels[robot])
        code.run()
        if code.running:
            panels[robot] = code.outputs.pop(0)
            painted_once.add(robot)

            turn = code.outputs.pop(0)
            if turn == 0:
                robot_dir = tuple(protccw(robot_dir))
            elif turn == 1:
                robot_dir = tuple(protcw(robot_dir))
            robot = tuple(padd(robot, robot_dir))

    if not part2:
        print('Part 1:', len(painted_once))
    else:
        print('Part 2:')
        print_grid(panels, {1: '\u2591\u2591', 0: '  '})


if __name__ == '__main__':
    puzzle_input = get_input_intcode()
    main(puzzle_input, False)
    main(puzzle_input, True)
