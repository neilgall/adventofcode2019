from itertools import groupby
from math import atan2, cos, pi, sin, sqrt


def load(text):
  """
  Given a textual map, yields all the asteroid coordinates
  """
  def generate():
    for y, row in enumerate(text.strip().split("\n")):
      for x, cell in enumerate(row):
        if cell == '#':
          yield (x, y)
  return list(generate())


def space_size(asteroids):
  """
  Given a sequence of asteroids, yields the search space size
  as a pair of xrange and yrange
  """
  minx = min(x for x,y in asteroids)
  maxx = max(x for x,y in asteroids)
  miny = min(y for x,y in asteroids)
  maxy = max(y for x,y in asteroids)
  return range(minx, maxx+1), range(miny, maxy+1)


def sq_distance_from(x1,y1):
  """
  Returns a function which determines the square of the distance
  from a point to (x1,y1)
  """
  def distance(p):
    x2, y2 = p
    return (x1-x2)**2 + (y1-y2)**2
  return distance


def gcd(x,y):
  """
  Greatest common divisor of x and y
  """
  return y if x == 0 else gcd(y % x, x)


def step_offset(sx,sy, ex,ey):
  """
  Calculate the step offset (x,y) to move from (sx,sy) to (ex,ey)
  hitting all integer locations in between
  """
  if ey == sy:
    return (1,0) if ex > sx else (-1,0)
  else:
    ox,oy = ex-sx, ey-sy
    min_step = gcd(abs(ox),abs(oy))
    return ox//min_step, oy//min_step


def best_location(asteroids):
  """
  Given a list of asteroids, determine the asteroid which can "see"
  the most other asteroids, and the number it can see
  """
  xrange, yrange = space_size(asteroids)
  best = None
  best_visible = 0

  for cx,cy in asteroids:
    visible = list(asteroids)
    visible.remove((cx,cy))

    for ax,ay in sorted(asteroids, key=sq_distance_from(cx,cy)):
      if (ax,ay) not in visible:
        continue
      ox, oy = step_offset(cx,cy,ax,ay)
      px,py = ax+ox,ay+oy
      while visible and px in xrange and py in yrange:
        if (px,py) in visible:
          visible.remove((px,py))
        px += ox
        py += oy

    if len(visible) > best_visible:
      best_visible = len(visible)
      best = cx, cy

  return (best, best_visible)


def vaporise_asteroids(asteroids, station):
  """
  Given the asteroid list and monitoring station location, yield the
  vaporised asteroids in order of destruction
  """
  def to_polar(x, y):
    """
    Convert cartesian coordinates to polar, rotated so that theta=0
    is directly upwards
    """
    theta = atan2(x, y)
    rho = sqrt(x**2 + y**2)
    return theta, rho


  def to_cartesian(polar):
    """
    Convert polar to cartesian coordinates, assuming theta=0 is
    directly upwards
    """
    theta, rho = polar
    oy, ox = round(rho * cos(theta)), round(rho * sin(theta))
    return sx + ox, sy + oy


  asteroids = (a for a in asteroids if a != station)
  sx, sy = station

  asteroids_polar = sorted((to_polar(ax - sx, ay - sy) for ax, ay in asteroids), reverse=True)
  asteroids_grouped = list((k,list(v)) for k,v in groupby(asteroids_polar, lambda a: a[0]))

  while asteroids_grouped != []:
    for theta, group in list(asteroids_grouped):
      yield to_cartesian(group[-1])
      if len(group) == 1:
        asteroids_grouped.remove((theta,group))
      else:
        del group[0]


tests = [
((3, 4), 8, """
.#..#
.....
#####
....#
...##
"""),
((5, 8), 33, """
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
"""),
((1,2), 35, """
#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.
"""),
((6,3), 41, """
.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..
"""),
((11,13), 210, """
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
""")
]

part2_test_map = """
.#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....X...###..
..#.#.....#....##
"""

def part1_tests():
  for pos, count, map in tests:
    p, c = best_location(load(map))
    assert (p,c) == (pos, count)

def part2_tests():
  v = vaporise_asteroids(load(part2_test_map), (8,3))
  assert next(v) == (8,1)
  assert next(v) == (9,0)
  assert next(v) == (9,1)
  assert next(v) == (10,0)
  assert next(v) == (9,2)
  assert next(v) == (11,1)
  assert next(v) == (12,1)
  assert next(v) == (11,2)
  assert next(v) == (15,1)


def part1(input):
  pos, count = best_location(input)
  return count


def part2(input):
  station, count = best_location(input)
  zap = vaporise_asteroids(input, station)
  x, y = list(zap)[199]
  return x*100+y

if __name__ == "__main__":
  part1_tests()
  part2_tests()

  with open("input.txt") as f:
 	  input = load(f.read())

  print(f"Part 1... {part1(input)}")
  print(f"Part 2... {part2(input)}")
