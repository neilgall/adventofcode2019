
orbits(com, b).
orbits(b, c).
orbits(c, d).
orbits(d, e).
orbits(e, f).
orbits(b, g).
orbits(g, h).
orbits(d, i).
orbits(e, j).
orbits(j, k).
orbits(k, l).

indirect(C, A) :- 
      orbits(C, A)
	; orbits(C, B), indirect(B, A).

total(N) :- 
	setof([X, Y], indirect(X, Y), S),
	length(S, N).
