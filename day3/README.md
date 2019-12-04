# Day 3

This was fairly complex. I knew from past AoC experience that the brute force method of enumerating every visited location would be computationally expensive so I wanted to solve it using the line segments as the basic abstraction. Plus that method extends fairly naturally to non-integer coordinate spaces, so feels like the right solution. One thing I try to do in AoC is find a solution that's production-ready, or nearly so.

The complexity and the fact I left it until the evening pushed me to Haskell for a clean, simple implementation. Not particularly proud of that `intersection` function with 12 variables in scope however.
