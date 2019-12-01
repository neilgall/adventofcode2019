using Test

load(file::AbstractString) = [parse(Int, line) for line in readlines(file)]

moduleFuel(mass::Int) = div(mass, 3) - 2

function fuelFuel(mass::Int)
  f = moduleFuel(mass)
  mass + if f <= 0 0 else fuelFuel(f) end
end

@testset "ModuleFuel" begin
  @test moduleFuel(12) == 2
  @test moduleFuel(14) == 2
  @test moduleFuel(1969) == 654
  @test moduleFuel(100756) == 33583
end

@testset "FuelFuel" begin
  @test fuelFuel(2) == 2
  @test fuelFuel(654) == 966
  @test fuelFuel(33583) == 50346
end

part1(modules) = sum(moduleFuel(m) for m in modules)

part2(modules) = sum(fuelFuel(moduleFuel(m)) for m in modules)

input = load("input.txt")
println("Part 1 : $(part1(input))")
println("Part 2 : $(part2(input))")