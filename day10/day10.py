
def load(text):
  """
  Given a textual map, yields all the asteroid coordinates
  """
  for y, row in enumerate(text.strip().split("\n")):
    for x, cell in enumerate(row):
      if cell == '#':
        yield (x, y)


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


def best_location(asteroids):
  """
  Given a list of asteroids, determine the asteroid which can "see"
  the most other asteroids, and the number it can see
  """
  asteroids = list(asteroids)
  xrange, yrange = space_size(asteroids)
  best = None
  best_visible = 0

  for cx,cy in asteroids:
    visible = list(asteroids)
    visible.remove((cx,cy))

    for ax,ay in sorted(asteroids, key=sq_distance_from(cx,cy)):
      if (ax,ay) not in visible:
        continue
      if ay == cy:
        ox,oy = (1,0) if ax > cx else (-1,0)
      else:
        ox,oy = ax-cx,ay-cy
        min_step = gcd(abs(ox),abs(oy))
        ox //= min_step
        oy //= min_step
      
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

if __name__ == "__main__":
  for pos, count, map in tests:
    p, c = best_location(load(map))
    assert (p,c) == (pos, count)

  with open("input.txt") as f:
 	  input = load(f.read())

  print(f"Part 1... {best_location(input)}")
