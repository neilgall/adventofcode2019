# Day 14

It's the weekend so time to stretch out and have some fun. I'm doing today's problem in Haskell.

The input has a more complex structure today so warrants a properly structured parser rather than ad hoc string splitting. But rather than just import Parsec I implemented my own small set of parser combinators.

Part 1 feels like a topological sort problem. If you sort the chemicals by dependencies starting at FUEL, then you can just walk the ordered list of chemicals adding the required amount to each input. The topological sort was straightforward as we've done these [before](https://github.com/neilgall/adventofcode2018/blob/master/src/day7/day7.kt) in AoC and I used my functional version of [Kahn's algorithm](https://en.wikipedia.org/wiki/Topological_sorting#Algorithms).

The next part had me scratching my head a bit about how the arithmetic works. I build a map of chemical name to quantity needed by walking the topologically sorted list of names, and at each step adding the scaled demand to each of the inputs. At the end the map has all the demanded quantities of each chemical so I just pull out the value for ORE.

## Part 2

At first I thought part 2 was going to reverse the search order so we started from ORE but I quickly realised the search space explodes once you can use an input for multiple outputs. It turned out much simpler - starting with an estimate of the quantity of ORE divided by the ORE needed for 1 FUEL, it's just a binary search to find the maximum amount of FUEL we can make.
