from dataclasses import dataclass, field
from typing import List
import pytest

@dataclass(frozen=True)
class Vector:
  x: int = 0
  y: int = 0
  z: int = 0

  def magnitude(self):
    return abs(self.x) + abs(self.y) + abs(self.z)

  def __add__(self, other):
    return Vector(self.x+other.x, self.y+other.y, self.z+other.z)


@dataclass(frozen=True)
class Moon:
  pos: Vector
  velocity: Vector = field(default_factory=Vector)


  def energy(self):
    return self.pos.magnitude() * self.velocity.magnitude()


def load(text):
  """
  Load multi-line `text` into a collection of Moons
  """
  def vector(line):
    parts = line.strip("<>").split(",")
    return Vector(*[int(p.strip()[2:]) for p in parts])

  return [Moon(pos=vector(line)) for line in text.strip().splitlines()]


def simulate(moons: List[Moon]) -> List[Moon]:
  """
  Simulate the motion of `moons`, returning a new collection of Moons
  """
  def compare_axis(x, y):
    if x < y: return 1
    elif x == y: return 0
    else: return -1

  def gravity_vector(a: Vector, b: Vector) -> Vector:
    return Vector(
      x = compare_axis(a.x, b.x),
      y = compare_axis(a.y, b.y),
      z = compare_axis(a.z, b.z)
    )

  def simulate_moon(m: Moon) -> Moon:
    gravity = Vector()
    for n in moons:
      gravity += gravity_vector(m.pos, n.pos)
    velocity = m.velocity + gravity
    return Moon(pos=m.pos + velocity, velocity=velocity)

  return [simulate_moon(m) for m in moons]


def simulate_n(moons: List[Moon], count: int) -> List[Moon]:
  """
  Simulate `moons` for `count` time steps
  """
  m = moons
  for i in range(count):
    m = simulate(m)
  return m


def total_energy(moons: List[Moon]) -> int:
  return sum(m.energy() for m in moons)


def test_load():
  """
  Verify parsing text into Moons
  """
  r = load("""<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>""")
  assert len(r) == 4
  assert r[0] == Moon(pos=Vector(-1, 0, 2))
  assert r[1] == Moon(pos=Vector(2,-10,-7))
  assert r[2] == Moon(pos=Vector(4,-8,8))
  assert r[3] == Moon(pos=Vector(3,5,-1))


EXAMPLE_MOONS_1 = [
  Moon(pos=Vector(-1,  0, 2)),
  Moon(pos=Vector( 2,-10,-7)),
  Moon(pos=Vector( 4, -8, 8)),
  Moon(pos=Vector( 3,  5,-1))
]

EXAMPLE_MOONS_2 = [
  Moon(pos=Vector(-8,-10,  0)),
  Moon(pos=Vector( 5,  5, 10)),
  Moon(pos=Vector( 2, -7,  3)),
  Moon(pos=Vector( 9, -8, -3))
]

@pytest.mark.parametrize("moons,steps,expect", [
  (EXAMPLE_MOONS_1, 1, [
    Moon(pos=Vector(2,-1, 1), velocity=Vector( 3,-1,-1)),
    Moon(pos=Vector(3,-7,-4), velocity=Vector( 1, 3, 3)),
    Moon(pos=Vector(1,-7, 5), velocity=Vector(-3, 1,-3)),
    Moon(pos=Vector(2, 2, 0), velocity=Vector(-1,-3, 1))
  ]),
  (EXAMPLE_MOONS_1, 2, [
    Moon(pos=Vector(5,-3,-1), velocity=Vector( 3,-2,-2)),
    Moon(pos=Vector(1,-2, 2), velocity=Vector(-2, 5, 6)),
    Moon(pos=Vector(1,-4,-1), velocity=Vector( 0, 3,-6)),
    Moon(pos=Vector(1,-4, 2), velocity=Vector(-1,-6, 2))
  ]),
  (EXAMPLE_MOONS_1, 10, [
    Moon(pos=Vector(2, 1,-3), velocity=Vector(-3,-2, 1)),
    Moon(pos=Vector(1,-8, 0), velocity=Vector(-1, 1, 3)),
    Moon(pos=Vector(3,-6, 1), velocity=Vector( 3, 2,-3)),
    Moon(pos=Vector(2, 0, 4), velocity=Vector( 1,-1,-1))
  ])
])
def test_simulate_one_step(moons, steps, expect):
  """
  Verify moon simulation
  """
  assert simulate_n(moons, steps) == expect



@pytest.mark.parametrize("moons,steps,energy", [
  (EXAMPLE_MOONS_1, 10, 179),
  (EXAMPLE_MOONS_2, 100, 1940)
])
def test_energy(moons, steps, energy):
  final = simulate_n(moons, steps)
  assert total_energy(final) == energy


def part1(moons):
  final = simulate_n(moons, 1000)
  print(f"Part 1 : {total_energy(final)}")


if __name__ == "__main__":
  with open("input.txt", "rt") as f:
    moons = load(f.read())

  part1(moons)
