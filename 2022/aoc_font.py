# Font recognition for the three (3) puzzles that have invoked it so far (2022 Day 10, 2021 Day 13, and 2018 Day 10)

from utils import FiniteGrid

LETTERS = {
    ('A', ('.##.', '#..#', '#..#', '####', '#..#', '#..#')),
    ('B', ('###.', '#..#', '###.', '#..#', '#..#', '###.')),
    ('C', ('.##.', '#..#', '#...', '#...', '#..#', '.##.')),
    ('E', ('####', '#...', '###.', '#...', '#...', '####')),
    ('F', ('####', '#...', '###.', '#...', '#...', '#...')),
    ('G', ('.##.', '#..#', '#...', '#.##', '#..#', '.###')),
    ('H', ('#..#', '#..#', '####', '#..#', '#..#', '#..#')),
    ('J', ('..##', '...#', '...#', '...#', '#..#', '.##.')),
    ('K', ('#..#', '#.#.', '##..', '#.#.', '#.#.', '#..#')),
    ('L', ('#...', '#...', '#...', '#...', '#...', '####')),
    ('P', ('###.', '#..#', '#..#', '###.', '#...', '#...')),
    ('R', ('###.', '#..#', '#..#', '###.', '#.#.', '#..#')),
    ('U', ('#..#', '#..#', '#..#', '#..#', '#..#', '.##.')),
    ('Z', ('####', '...#', '..#.', '.#..', '#...', '####'))
}

HEIGHT = 6
WIDTH = 4


def recognize(grid: FiniteGrid, offset_x: int = 0, offset_y: int = 0) -> str:
    width, height = grid.width - offset_x, grid.height - offset_y

    assert (width + 1) % (WIDTH + 1) == 0, 'Uneven amount of letters: width %d' % width
    assert height == HEIGHT, 'Font height %d != 6' % height

    dx = 0
    letters = []
    while dx < width:
        for letter, pattern in LETTERS:
            if dx + WIDTH <= width:  # This letter can fit in the provided width
                if all((pattern[y][x] == '#') == (grid[offset_x + dx + x, offset_y + y] == '#')
                       for x in range(WIDTH)
                       for y in range(HEIGHT)):
                    letters.append(letter)
                    dx += WIDTH + 1
                    break
        else:
            raise ValueError('No matching text found! Found start = %s at offset dx = %d' % (letters, dx))
    return ''.join(letters)