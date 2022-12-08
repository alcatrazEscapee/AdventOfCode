# Day 8: Treetop Tree House
# Rank: 208 / 117

from utils import get_input, FiniteGrid
from typing import Tuple

def main(text: str):
    grid = FiniteGrid.of_str(text, default=-1).map_values(int)
    visible = 0
    score = 0

    assert grid.height == grid.width
    for x, y in grid.locations():
        if x == 0 or y == 0 or x == grid.width - 1 or y == grid.height - 1:
            visible += 1  # Exterior points are visible
        else:
            tree_visible, tree_score = False, 1
            for dx, dy in ((1, 0), (0, 1), (-1, 0), (0, -1)):
                v, d = project(grid, x, y, dx, dy)
                tree_visible |= v
                tree_score *= d

            if tree_visible:
                visible += 1
            if tree_score > score:
                score = tree_score

    print('Part 1:', visible)
    print('Part 2:', score)

def project(grid: FiniteGrid, x: int, y: int, dx: int, dy: int) -> Tuple[bool, int]:
    tree = grid[x, y]
    view = 0
    for i in range(1, grid.width):
        adj = grid[x + i * dx, y + i * dy]
        if adj >= tree:
            return False, view + 1  # View blocked, but we can see the tree we're blocked by
        if adj == -1:
            return True, view  # View unblocked since we reached the edge
        view += 1


if __name__ == '__main__':
    main(get_input(8))
