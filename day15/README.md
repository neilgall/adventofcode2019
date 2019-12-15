# Day 15

Another IntCode problem! But the description is all about communication with a remote control program - let's do that for real! For the first step I've connected the IntCode computer's Input/Output instructions to a socket. I'll write the problem solving program in another language.

Part 1 is a depth-first search, with the added complication that finding out anything new about the search space requires talking to the repair droid over a very simple interface. I of couse made that much harder for myself by writing a search algorithm that has to run in the IO monad.

Part 2 is a classic "flood fill", as you might have written in a paint program in the 1980s. Maybe they still use similar algorithms.

To run this one,
```
$ gcc -o day15 day15.c
$ ./day15 &
$ runhaskell day15.hs
```
