# Day 22: Monkey Map
# Rank: 29 / 3917 = After Sleep :P

from utils import get_input, FiniteGrid, Point2
from typing import NamedTuple, Tuple, Dict, List


def main(text: str):
    net_lines, rules = parse(text)
    print('Part 1:', part1(net_lines, rules))
    print('Part 2:', part2(net_lines, rules, 50))

def example(text: str):
    net_lines, rules = parse(text)
    assert 6032 == part1(net_lines, rules)
    assert 5031 == part2(net_lines, rules, 4)


def parse(text: str) -> Tuple[List[str], List[str]]:
    *net_lines, _, rules = text.split('\n')
    rules = rules.replace('R', ' R ').replace('L', ' L ').strip().split(' ')
    return net_lines, rules

def password(x: int, y: int, direction: Point2) -> int:
    return (y + 1) * 1000 + (x + 1) * 4 + abs(direction.x) * (1 - direction.x) + abs(direction.y) * (2 - direction.y)


def part1(lines: List[str], rules: List[str]) -> int:
    grid = FiniteGrid.of_iter([
        (('%%-%ds' % max(map(len, lines))) % line.replace(' ', '@')).replace(' ', '@')
        for line in lines], default='@')

    # 'You begin the path in the leftmost open tile of the top row of tiles.'
    # Today, doing exactly what the question suggests is actually a very good way to do things
    pos = Point2(0, 0)
    while grid[pos] == '@':
        pos += RIGHT

    # 'Initially, you are facing to the right', okay
    direction = RIGHT

    for rule in rules:
        if rule == 'L':
            direction = direction.ccw()
        elif rule == 'R':
            direction = direction.cw()
        else:
            for _ in range(int(rule)):
                next_pos = pos + direction
                if grid[next_pos] == '@':
                    # And I quote, 'if your next tile is off of the board, you should instead look in the direction opposite
                    #   of your current facing as far as you can until you find the opposite edge of the board, then reappear there.'
                    # Surprisingly, this works really well
                    next_pos = pos
                    while grid[next_pos] != '@':
                        next_pos -= direction
                    next_pos += direction  # Jump back onto the map

                if grid[next_pos] == '#':
                    break  # continue with next rule
                else:
                    pos = next_pos

    return password(pos.x, pos.y, direction)


def part2(net_lines: List[str], rules: List[str], width: int):
    """
    Magic Cube Folding!

    Given a net, this performs the following steps:
    - subdivides the grid represented by `lines` into a spare (N*width) x (M*width) grid of populated squares
    - numbers each face left->right, top->bottom, from 1 to 6
    - starting from face 1 as the 'front', in a BFS style pattern, begins folding the net together, building and rotating a cube as it does so
    - enumerates the edges of the cube, translating each edge back into the original orientation of the net's face
    - translates each edge pair into a list of points that are adjacent when moving from one edge to the other
    - executes the travel rules as previously, but using the above point mapping to jump from edge points instead
    """

    net_width = max(len(line) for line in net_lines)
    net_height = len(net_lines)

    # Label the faces from left to right, top to bottom
    # Starting position is always on face 0, top left (0, 0)
    faces_by_pos: Dict[Tuple[int, int], Face] = {}  # Used to build the cube based on the net positions
    grids_by_id: Dict[int, FiniteGrid] = {}  # Used to index into the grid while walking it
    nets_by_id: Dict[int, Point2] = {}  # Used to calculate the absolute net-based position, for calculating the final answer
    origin: Point2 | None = None
    origin_face: Face | None = None
    for net_y in range(0, net_height, width):
        for net_x in range(0, net_width, width):
            net_face_lines = [line[net_x:net_x + width] for line in net_lines[net_y:net_y + width]]
            if any('.' in line for line in net_face_lines):
                face = Face.identity(len(faces_by_pos) + 1)
                x, y = net_x // width, net_y // width
                faces_by_pos[x, y] = face
                grids_by_id[face.id] = FiniteGrid.of_iter([line[net_x:net_x + width] for line in net_lines[net_y:net_y + width]])
                nets_by_id[face.id] = Point2(net_x, net_y)
                if origin is None:
                    origin = Point2(net_x // width, net_y // width)
                    origin_face = face

    # Performs the main folding BFS, in a recursive fashion
    # Iteratively rotates the cube, adds a new face, recursively continues, and then reverts the rotations on the way up
    # At the end, `cube.edges()` matches up each edge and face ID pair
    def fold(cube: Cube, net_pos: Point2) -> Cube:
        for direction in (UP, DOWN, LEFT, RIGHT):
            if (key := (net_pos + direction)) in faces_by_pos and cube.at(direction):
                cube = fold(cube.rotate(direction).set(faces_by_pos[key]), key).rotate(-direction)
        return cube

    # Fold the cube up
    cube = fold(Cube.identity().set(origin_face), origin)

    # And iterate each edge, iterate the points, and match them up
    # The orientation of each edge pair is deterministic and can be calculated from just the edges being matched
    net_points: Dict[Tuple[int, Point2, Point2], Tuple[int, Point2, Point2]] = {}
    for (src_id, src_edge, dest_id, dest_edge) in cube.edges():
        # This is a handed-ness check, essentially, that checks based on the orientation of the edges, if the coordinates need to be iterated in reverse order
        flip = (src_edge == UP or src_edge == RIGHT) == (dest_edge == UP or dest_edge == RIGHT)
        for src_pos, dest_pos in zip(
            points(width, src_edge),
            points(width, dest_edge, flip)
        ):
            net_points[src_id, src_pos, src_edge] = dest_id, dest_pos, -dest_edge
            net_points[dest_id, dest_pos, dest_edge] = src_id, src_pos, -src_edge

    # Now execute the rules, and use `net_points` to manage traversals from one grid to another
    pos: Point2 = Point2(0, 0)
    grid: int = 1
    direction: Point2 = RIGHT
    for rule in rules:
        if rule == 'L':
            direction = direction.ccw()
        elif rule == 'R':
            direction = direction.cw()
        else:
            for _ in range(int(rule)):
                next_grid, next_pos, next_direction = grid, pos + direction, direction
                if next_pos not in grids_by_id[grid]:
                    next_grid, next_pos, next_direction = net_points[grid, pos, direction]
                if grids_by_id[next_grid][next_pos] == '#':
                    break
                else:
                    grid, pos, direction = next_grid, next_pos, next_direction

    net = nets_by_id[grid]
    return password(net.x + pos.x, net.y + pos.y, direction)


def points(width: int, edge: Point2, flip: bool = False) -> List[Point2]:
    if flip:
        return points(width, edge, False)[::-1]
    if edge == UP:
        return [Point2(x, 0) for x in range(width)]
    if edge == DOWN:
        return [Point2(x, width - 1) for x in range(width)]
    if edge == LEFT:
        return [Point2(0, y) for y in range(width)]
    if edge == RIGHT:
        return [Point2(width - 1, y) for y in range(width)]


class Face(NamedTuple):
    """
    The directions map from the reference frame of the cube, to the original reference frame.
    That is to say, if this is on a given face of the cube, based on convention, a specific edge will be identified as the 'up' edge of that face
    That face's 'up' property then identifies which edge, on the *flat net* matches that.

    Given that, the various `rotate` methods on this `Face`, are rotating the reference frame of the cube.
    """

    @staticmethod
    def identity(face_id: int) -> 'Face':
        return Face(face_id, UP, RIGHT, DOWN, LEFT)

    id: int
    up: Point2
    right: Point2
    down: Point2
    left: Point2

    def rotate_cw(self) -> 'Face': return Face(self.id, self.left, self.up, self.right, self.down)
    def rotate_ccw(self) -> 'Face': return Face(self.id, self.right, self.down, self.left, self.up)
    def rotate_180(self) -> 'Face': return Face(self.id, self.down, self.left, self.up, self.right)


class Cube(NamedTuple):
    """
    The faces of this cube are oriented where 'up' for horizontal faces is always 'up', for 'down', 'up' points towards the front, and for 'up', 'up' points towards the back.
    The rotation methods rotate so that the given face is the new front, i.e. `rotate_up` causes the current `up` to be the new `front` on the returned cube.
    """

    @staticmethod
    def identity() -> 'Cube':
        return Cube(Face.identity(-1), Face.identity(-1), Face.identity(-1), Face.identity(-1), Face.identity(-1), Face.identity(-1))

    front: Face
    back: Face
    left: Face
    right: Face
    up: Face
    down: Face

    def at(self, direction: Point2) -> bool: return {UP: self.up, DOWN: self.down, LEFT: self.left, RIGHT: self.right}[direction].id == -1
    def set(self, front: Face) -> 'Cube': return Cube(front, self.back, self.left, self.right, self.up, self.down)
    def rotate(self, direction: Point2) -> 'Cube': return {UP: self.rotate_up, DOWN: self.rotate_down, LEFT: self.rotate_left, RIGHT: self.rotate_right}[direction]()

    def rotate_up(self) -> 'Cube': return Cube(self.up, self.down.rotate_180(), self.left.rotate_cw(), self.right.rotate_ccw(), self.back.rotate_180(), self.front)
    def rotate_down(self) -> 'Cube': return Cube(self.down, self.up.rotate_180(), self.left.rotate_ccw(), self.right.rotate_cw(), self.front, self.back.rotate_180())
    def rotate_left(self) -> 'Cube': return Cube(self.left, self.right, self.back, self.front, self.up.rotate_ccw(), self.down.rotate_cw())
    def rotate_right(self) -> 'Cube': return Cube(self.right, self.left, self.front, self.back, self.up.rotate_cw(), self.down.rotate_ccw())

    def edges(self) -> List[Tuple[int, Point2, int, Point2]]:
        # Enumerates the edge matchups, converting each edge to the net reference frame using the face's direction fields.
        # Returns a list of pairs of a face ID, and a edge direction, each pair of which forms a single edge
        return [(x.id, f, y.id, g) for x, f, y, g in [
            (self.front, self.front.up, self.up, self.up.down),
            (self.front, self.front.down, self.down, self.down.up),
            (self.front, self.front.left, self.left, self.left.right),
            (self.front, self.front.right, self.right, self.right.left),
            (self.left, self.left.up, self.up, self.up.left),
            (self.left, self.left.down, self.down, self.down.left),
            (self.right, self.right.up, self.up, self.up.right),
            (self.right, self.right.down, self.down, self.down.right),
            (self.back, self.back.up, self.up, self.up.up),
            (self.back, self.back.down, self.down, self.down.down),
            (self.back, self.back.left, self.right, self.right.right),
            (self.back, self.back.right, self.left, self.left.left),
        ]]


RIGHT = Point2(1, 0)
LEFT = Point2(-1, 0)
UP = Point2(0, -1)
DOWN = Point2(0, 1)


if __name__ == '__main__':
    example(get_input(22, '../inputs/day%02d_example.txt'))
    main(get_input(22))
