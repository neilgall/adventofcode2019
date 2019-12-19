import socket
import subprocess
import sys
import time
from enum import Enum
from collections import defaultdict
from contextlib import contextmanager
from itertools import chain, permutations


class channel:
    def __init__(self, host, port):
        self._host = host
        self._port = port

    def __enter__(self):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._sock = sock.__enter__()
        self._sock.connect((self._host, self._port))
        self._rxbuf = bytes()
        self._pos = 0
        self._eof = False
        return self

    def __exit__(self, *args):
        print(exit)
        self._sock.__exit__(*args)


    def peek_byte(self):
        if self._pos >= len(self._rxbuf):
            self._rxbuf = self._sock.recv(1024)
            self._pos = 0
        if self._pos >= len(self._rxbuf):
            self._eof = True
        else:
            return chr(self._rxbuf[self._pos])

    def get_byte(self):
        r = self.peek_byte()
        self._pos += 1
        return r


    def receive_image(self):
        image = ""
        while not self._eof and self.peek_byte() in ['.','#','<','>','^','v','\n']:
            image += self.get_byte()

        return image.strip().split("\n")

    def send(self, msg):
        self._sock.send(f"{msg}\n".encode('ascii'))

    def receive_str(self):
        m = ""
        while not m.endswith("\n"):
            m += "%c" % self.get_byte()
        return m.strip()

    def expect(self, msg):
        m = self.receive_str()
        assert m.endswith(msg)


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
    if y == 0 or y == len(image)-1: return False
    if x == 0 or x == len(image[y])-1: return False
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
        if x2 < x1: return Turn.LEFT
        if x2 > x1: return Turn.RIGHT
    if d == Dir.SOUTH:
        if x2 > x1: return Turn.LEFT
        if x2 < x1: return Turn.RIGHT
    if d == Dir.EAST:
        if y2 < y1: return Turn.LEFT
        if y2 > y1: return Turn.RIGHT
    if d == Dir.WEST:
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


def trace_routes(image):
    scaffold_count = sum(int(is_scaffold(image, x, y)) for x,y in scan(image))

    def can_go_to(x, y, visited):
        if not in_range(image, x, y) or not is_scaffold(image, x, y):
            return False
        else:
            return is_intersection(image, x, y) or not (x,y) in visited


    def go_forward(rx, ry, rd, visited):
        visited = set(visited)
        moved = 0
        moved_since_intersection = 0
        intersections = []
        while True:
            mx, my = move(rx, ry, rd)
            if not can_go_to(mx, my, visited):
                break
            moved += 1
            moved_since_intersection += 1
            rx, ry = mx, my
            visited.add((rx, ry))
            if is_intersection(image, rx, ry):
                intersections.append((moved, rx, ry, set(visited)))
                moved_since_intersection = 0

        if moved_since_intersection > 0:
            yield moved, rx, ry, visited

        yield from intersections


    def find_turns(rx, ry, rd, visited):
        for nx,ny in neighbours(image, rx, ry):
            if can_go_to(nx, ny, visited):
                turn = find_turn(rx, ry, rd, nx, ny)
                if turn:
                    yield turn


    def routes_from(rx, ry, rd, visited):
        visited = set(visited)
        extend = False
        for turn in find_turns(rx, ry, rd, visited):
            td = apply_turn(rd, turn)
            for moved, mx, my, mvisited in go_forward(rx, ry, td, visited):
                extend = True
                yield from ([turn.value, moved, *r] for r in routes_from(mx, my, td, mvisited))

        if not extend and len(visited) == scaffold_count:
            yield from [[]]

    rx, ry, rd = find_robot(image)
    yield from routes_from(rx, ry, rd, set())


def sections(route):
    for section in range(0, len(route)-1, 2):
        for section_len in range(2, len(route)-section, 2):
            yield route[section:section+section_len]


def render(section):
    return ",".join(str(s) for s in section)


def by_frequency(sections):
    freqs = defaultdict(int)
    for s in sections:
        r = render(s)
        if len(r) <= 20:
            freqs[r] += 1

    def order(i):
        return -1 if i[1] == 1 else len(i[0])*i[1]

    ordered = set(sorted(freqs.items(), key=order))
    yield from (r for r,f in ordered)


def compress(route):
    route_sections = list(sections(route))
    functions = ['A', 'B', 'C']

    def all_functions(r):
        return all(x in functions for x in r.split(","))

    def find_replacements(compressed, func_defs, sections):
        name = functions[len(func_defs)]
        for fix in sections:
            next_func_defs = { **func_defs, name: fix }
            next_compressed = compressed.replace(fix, name)
            if not len(next_compressed) < len(compressed):
                continue

            if all_functions(next_compressed) and len(next_compressed) <= 20:
                yield next_compressed, next_func_defs

            elif len(next_func_defs) < len(functions):
                unfixed = list(sections)
                unfixed.remove(fix)
                yield from find_replacements(next_compressed, next_func_defs, unfixed)

    def overall_length(compressed):
        c, fs = compressed
        return len(c) + sum(len(v) for v in fs.values())

    top_sections = list(by_frequency(route_sections))
    replacements = find_replacements(render(route), {}, top_sections)
    return min(replacements, key=overall_length)


@contextmanager
def run_intcode(mode):
    proc = subprocess.Popen(["./day17", str(mode)], stdout=subprocess.DEVNULL)
    time.sleep(1)
    with channel("localhost", 5000) as chan:
        yield chan
    proc.terminate()


def part1():
    with run_intcode(1) as chan:
        image = chan.receive_image()
        total = sum(find_alignment_parameters(image))
        print(f"Part 1 .. {total}")


def part2():
    with run_intcode(2) as chan:
        image = chan.receive_image()
        for route in trace_routes(image):
            print(f"route: {render(route)}")
            main, funcs = compress(route)
            print(f"main: {main}")
            for name, f in funcs.items():
                print(f"{name}: {f}")
            break

        chan.expect("Main:")
        chan.send(main)
        for name, func in funcs.items():
            chan.expect(f"Function {name}:")
            chan.send(func)

        chan.expect("Continuous video feed?")
        chan.send("N")

        image = chan.receive_image()
        dust = chan.receive_str()
        print(f"Dust Collected: {dust}")



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

def test_part2():
    for route in trace_routes(test_image):
        print(f"route: {render(route)}")
        main, funcs = compress(route)
        print(f"main: {main}")
        for name, f in funcs.items():
            print(f"{name}: {f}")


if __name__ == "__main__":
    if len(sys.argv) > 1:
        test_part2()
    else:
        part1()
        part2()
