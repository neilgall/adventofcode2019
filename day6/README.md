# Day 6

Ok back to Prolog because part 1 at least is really easy. The orbits represent a dependency graph between objects, as illustrated in question. Prolog is perfectly suited to searching such a dependency graph with one simple rule: C orbits A indirectly if it orbits it directly or if it orbits another object which orbits A indirectly. This is actually easier to express in Prolog than English:

	indirect(C, A) :- 
	      orbits(C, A)
		; orbits(C, B), indirect(B, A).

The total number of indirect orbits is simply the count of results for which this holds true.

Rather than work out how to read and parse data in Prolog I just turned the input rules into a Prolog source file using Perl. Is that cheating?
