import socket
from enum import Enum
from collections import defaultdict

def receive_image(s):
    image = ""
    while True:
        r = s.recv(1024)
        if r:
            for b in r:
                image += '%c' % (b)
        else:
            break
    return image.strip().split("\n")


class Dir(Enum):
    NORTH = '^'
    SOUTH = 'v'
    WEST = '<'
    EAST = '>'


class Turn(Enum):
    LEFT = 'L'
    RIGHT = 'R'
    NONE = 'N'


def in_range(image, x, y):
    if y < 0 or len(image) <= y: return False
    if x < 0 or len(image[y]) <= x: return False
    return True


def is_scaffold(image, x, y):
    return image[y][x] == '#'


def is_robot(image, x, y):
    return image[y][x] in list(d.value for d in Dir)


def scan(image):
    for y in range(0, len(image)):
        line = image[y]
        for x in range(0, len(line)):
            yield x,y


def neighbours(image, x, y):
    if 1 < y: yield x, y-1
    if 1 < x: yield x-1, y
    if y < len(image)-1: yield x, y+1
    if x < len(image[y])-1: yield x+1, y


def is_intersection(image, x, y):
    return all(is_scaffold(image, nx, ny) for nx, ny in neighbours(image, x, y))


def find_intersections(image):
    for x,y in scan(image):
        if is_intersection(image, x, y):
            yield x,y


def find_alignment_parameters(image):
    for x,y in find_intersections(image):
        yield x * y


def find_robot(image):
    for x,y in scan(image):
        if is_robot(image, x, y):
            return x, y, Dir(image[y][x])


def move(x, y, d):
    if d == Dir.NORTH: return x, y-1
    if d == Dir.SOUTH: return x, y+1
    if d == Dir.EAST: return x+1, y
    if d == Dir.WEST: return x-1, y


def find_turn(x1, y1, d, x2, y2):
    if d == Dir.NORTH:
        if y2 < y1: return Turn.NONE
        if x2 < x1: return Turn.LEFT
        if x2 > x1: return Turn.RIGHT
    if d == Dir.SOUTH:
        if y2 > y1: return Turn.NONE
        if x2 > x1: return Turn.LEFT
        if x2 < x1: return Turn.RIGHT
    if d == Dir.EAST:
        if x2 > x1: return Turn.NONE
        if y2 < y1: return Turn.LEFT
        if y2 > y1: return Turn.RIGHT
    if d == Dir.WEST:
        if x2 < x1: return Turn.NONE
        if y2 > y1: return Turn.LEFT
        if y2 < y1: return Turn.RIGHT


def apply_turn(d, t):
    if t == Turn.NONE:
        return d
    if t == Turn.LEFT:
        if d == Dir.NORTH: return Dir.WEST
        if d == Dir.SOUTH: return Dir.EAST
        if d == Dir.WEST: return Dir.SOUTH
        if d == Dir.EAST: return Dir.NORTH
    if t == Turn.RIGHT:
        if d == Dir.NORTH: return Dir.EAST
        if d == Dir.SOUTH: return Dir.WEST
        if d == Dir.WEST: return Dir.NORTH
        if d == Dir.EAST: return Dir.SOUTH


def trace_route(image):
    rx, ry, rd = find_robot(image)
    visited = set((rx,ry))
    moved = 0

    def can_travel(x, y):
        return is_scaffold(image, x, y) and \
            (not (x,y) in visited or is_intersection(image, x, y))

    while True:
        mx, my = move(rx, ry, rd)
        if in_range(image, mx, my) and is_scaffold(image, mx, my):
            moved += 1
            rx, ry = mx, my
            visited.add((rx,ry))
        else:
            ns = list((nx,ny) for nx,ny in neighbours(image, rx, ry) if can_travel(nx, ny))
            if not ns:
                if moved > 0: yield moved
                return
            else:
                nx, ny = ns[0]
                turn = find_turn(rx, ry, rd, nx, ny)
                rd = apply_turn(rd, turn)
                if moved > 0: yield moved
                yield turn.value
                moved = 0


def sections(route):
    for section in range(0, len(route)-1):
        for section_len in range(2, len(route)-section):
            yield route[section:section+section_len]


def render(section):
    return ",".join(str(s) for s in section)


def compress(route):
    def order(i):
        return -1 if i[1] == 1 else len(i[0])

    freqs = defaultdict(int)
    for s in sections(route):
        r = render(s)
        if len(r) <= 20:
            freqs[r] += 1
    for s,f in sorted(freqs.items(), key=order, reverse=True):
        print(s,f)


def part1(image):
    total = sum(find_alignment_parameters(image))
    print(f"Part 1 .. {total}")


def part2(image):
    route = list(trace_route(image))
    print(route)
    compress(route)
    # print(",".join(str(r) for r in trace_route(image)))


test_image = """
#######...#####
#.....#...#...#
#.....#...#...#
......#...#...#
......#...###.#
......#.....#.#
^########...#.#
......#.#...#.#
......#########
........#...#..
....#########..
....#...#......
....#...#......
....#...#......
....#####......
""".strip().split("\n")


if __name__ == "__main__":
    part1(test_image)
    part2(test_image)
    # with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
    #     s.connect(("localhost", 5000))
    #     image = receive_image(s)
    #     part1(image)
