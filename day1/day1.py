from typing import Sequence

def load(file):
    with open(file, "rt") as f:
        return list(int(line) for line in f.readlines())

def moduleFuel(mass: int) -> int:
    return (mass // 3) - 2

def fuelFuel(mass: int) -> int:
    f = moduleFuel(mass)
    return mass + (0 if f <= 0 else fuelFuel(f))

def testModuleFuel():
    assert(moduleFuel(12) == 2)
    assert(moduleFuel(14) == 2)
    assert(moduleFuel(1969) == 654)
    assert(moduleFuel(100756) == 33583)

def testFuelFuel():
    assert(fuelFuel(2) == 2)
    assert(fuelFuel(654) == 966)
    assert(fuelFuel(33583) == 50346)


def part1(modules: Sequence[int]) -> int:
    return sum(moduleFuel(f) for f in modules)

def part2(modules: Sequence[int]) -> int:
    return sum(fuelFuel(moduleFuel(f)) for f in modules)

if __name__ == "__main__":
    testModuleFuel()
    testFuelFuel()
    input = load("input.txt")
    print(f"Part 1 : {part1(input)}")
    print(f"Part 2 : {part2(input)}")
