
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

can_jump(X, Y) :-
	  orbits(X, Y)
	; orbits(Y, X).	

shortest_path_to(X, Y, L) :-
	  path_to(X, Y, P)
	, length(P, L).

path_to(X, Y, P) :-
	path_to_impl(X, Y, [X], P).

path_to_impl(X, Y, _, [Y]) :-
	can_jump(X, Y).

path_to_impl(X, Y, L, [Z|Path]) :-
	  can_jump(X, Z)
	, \+(member(Z, L))
	, \+(member(Y, L))
	, path_to_impl(Z, Y, [Z|L], Path).
