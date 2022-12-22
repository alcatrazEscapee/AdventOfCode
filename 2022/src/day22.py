# Day 22: Monkey Map
# Rank: 29 / 3917 = After Sleep :P

from utils import get_input, FiniteGrid, Point2
from typing import NamedTuple, Tuple, Dict, List


class NetFace(NamedTuple):
    id: int
    x: int
    y: int
    grid: FiniteGrid


def main(text: str):
    parsed = parse(text)
    print('Part 1:', part1(parsed))
    print('Part 2:', part2(parsed, 50, {
        (1, LEFT, DOWN): (4, LEFT, UP),
        (1, UP, RIGHT): (6, LEFT, DOWN),
        (2, UP, RIGHT): (6, DOWN, RIGHT),
        (2, DOWN, RIGHT): (3, RIGHT, DOWN),
        (2, RIGHT, DOWN): (5, RIGHT, UP),
        (3, LEFT, DOWN): (4, UP, RIGHT),
        (5, DOWN, RIGHT): (6, RIGHT, DOWN),
    }))


def parse(text: str) -> Tuple[List[str], List[str]]:
    *net_lines, _, rules = text.split('\n')
    rules = rules.replace('R', ' R ').replace('L', ' L ').strip().split(' ')
    return net_lines, rules


def part1(parsed: Tuple[List[str], List[str]]) -> int:
    lines, rules = parsed
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


def part2(parsed: Tuple[List[str], List[str]], width: int, nets: Dict[Tuple[int, Point2, Point2], Tuple[int, Point2, Point2]]) -> int:
    net_lines, rules = parsed
    net_width = max(len(line) for line in net_lines)
    net_height = len(net_lines)

    # Label the faces from left to right, top to bottom
    # Starting position is always on face 0, top left (0, 0)
    faces = [None]
    for net_y in range(0, net_height, width):
        for net_x in range(0, net_width, width):
            net_face_lines = [line[net_x:net_x + width] for line in net_lines[net_y:net_y + width]]
            if any('.' in line for line in net_face_lines):
                net_face = FiniteGrid.of_iter([line[net_x:net_x + width] for line in net_lines[net_y:net_y + width]], default='@')
                faces.append(NetFace(len(faces), net_x, net_y, net_face))

    # Identify internal edges to the net. These are not specified by the input as we can find them relatively straightforwardly by enumerating possible edges
    for src_face in faces[1:]:
        for dest_face in faces[1:]:
            if src_face.x + width == dest_face.x and src_face.y == dest_face.y:  # Adjacent left-right edge
                nets[src_face.id, RIGHT, DOWN] = dest_face.id, LEFT, DOWN
            if src_face.x == dest_face.x and src_face.y + width == dest_face.y:  # Adjacent up-down edge
                nets[src_face.id, DOWN, RIGHT] = dest_face.id, UP, RIGHT

    # A net is composed of a set of relations between faces
    # a relation is defined by (origin face) -> {(origin edge, direction along it) -> (destination edge, direction along it)}
    # note that relations are bidirectional, so we first build and append the inverse relationships.
    nets.update({q: p for p, q in nets.items()})

    # We then expand each net into all possible points along that edge. This is where we handle specific directions of edge matching
    # For example, a net (1, UP, RIGHT) specifies that for net (1, UP), points (0, 0) to (width - 1, 0) are sources.
    # Finally, note that the direction used to identify the edge now also becomes the direction of travel in and out
    net_points: Dict[Tuple[int, Point2, Point2], Tuple[int, Point2, Point2]] = {}
    for (src_net, src_edge, src_dir), (dest_net, dest_edge, dest_dir) in nets.items():
        for src_pos, dest_pos in zip(
            points_in_edge(width, src_edge, src_dir),
            points_in_edge(width, dest_edge, dest_dir)
        ):
            net_points[src_net, src_pos, src_edge] = dest_net, dest_pos, -dest_edge

    pos: Point2 = Point2(0, 0)
    face: NetFace = faces[1]
    direction: Point2 = RIGHT
    for rule in rules:
        if rule == 'L':
            direction = direction.ccw()
        elif rule == 'R':
            direction = direction.cw()
        else:
            for _ in range(int(rule)):
                next_face, next_pos, next_direction = face.id, pos + direction, direction
                if face.grid[next_pos] == '@':
                    next_face, next_pos, next_direction = net_points[face.id, pos, direction]
                if faces[next_face].grid[next_pos] == '#':
                    break
                else:
                    face, pos, direction = faces[next_face], next_pos, next_direction

    return password(face.x + pos.x, face.y + pos.y, direction)


def points_in_edge(width: int, edge: Point2, direction: Point2):
    if direction == UP or direction == LEFT:
        return points_in_edge(width, edge, -direction)[::-1]
    if edge == UP:
        return [Point2(x, 0) for x in range(width)]
    if edge == DOWN:
        return [Point2(x, width - 1) for x in range(width)]
    if edge == LEFT:
        return [Point2(0, y) for y in range(width)]
    if edge == RIGHT:
        return [Point2(width - 1, y) for y in range(width)]

def password(x: int, y: int, direction: Point2) -> int:
    return (y + 1) * 1000 + (x + 1) * 4 + abs(direction.x) * (1 - direction.x) + abs(direction.y) * (2 - direction.y)


RIGHT = Point2(1, 0)
LEFT = Point2(-1, 0)
UP = Point2(0, -1)
DOWN = Point2(0, 1)


if __name__ == '__main__':
    assert part2(parse(get_input(22, '../inputs/day%02d_example.txt')), 4, {
        (1, LEFT, DOWN): (3, UP, RIGHT),
        (1, UP, RIGHT): (2, UP, LEFT),
        (1, RIGHT, DOWN): (6, RIGHT, UP),
        (2, LEFT, DOWN): (6, DOWN, LEFT),
        (2, DOWN, RIGHT): (5, DOWN, LEFT),
        (3, DOWN, RIGHT): (5, LEFT, UP),
        (4, RIGHT, DOWN): (6, UP, LEFT),
    }) == 5031

    main(get_input(22))