# Day 6

Ok back to Prolog because part 1 at least is really easy. The orbits represent a dependency graph between objects, as illustrated in question. Prolog is perfectly suited to searching such a dependency graph with one simple rule: C orbits A indirectly if it orbits it directly or if it orbits another object which orbits A indirectly. This is actually easier to express in Prolog than English:

	indirect(C, A) :- 
	      orbits(C, A)
		; orbits(C, B), indirect(B, A).

The total number of indirect orbits is simply the count of results for which this holds true.

Rather than work out how to read and parse data in Prolog I just turned the input rules into a Prolog source file using Perl. Is that cheating?


## Part 2

Part 2 boils down to finding the shortest path in a graph. I had to research a bit to remember how to correctly recurse in Prolog building up a result. The trick is you need to build a separate list of the places visited so you can eliminate routes which visit the same place twice. The actual algorithm is then similar to part1 - there's a path from X to Y if you can jump directly between them, or there's a path from X to Y via Z if you can jump to Z and there's a path from Z to Y.

	path_to(X, Y, P) :-
		path_to_impl(X, Y, [X], P).

	path_to_impl(X, Y, _, [Y]) :-
		can_jump(X, Y).

	path_to_impl(X, Y, L, [Z|Path]) :-
		  can_jump(X, Z)
		, \+(member(Z, L))
		, \+(member(Y, L))
		, path_to_impl(Z, Y, [Z|L], Path).
