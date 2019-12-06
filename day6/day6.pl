
include(rules).

indirect(C, A) :- 
      orbits(C, A)
	; orbits(C, B), indirect(B, A).

total(N) :- 
	setof([X, Y], indirect(X, Y), S),
	length(S, N).
