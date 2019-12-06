
include(rules).

indirect(C, A) :- 
      orbits(C, A)
	; orbits(C, B), indirect(B, A).

total(N) :- 
	setof([X, Y], indirect(X, Y), S),
	length(S, N).

can_jump(X, Y) :-
	  orbits(X, Y)
	; orbits(Y, X).	

part2(Length) :-
	findall(L, (
		  orbits(SAN, object_SAN)
		, orbits(YOU, object_YOU)
		, path_to(YOU, SAN, P)
		, length(P, L)
	), Length).

path_to(X, Y, P) :-
	path_to_impl(X, Y, [X], P).

path_to_impl(X, Y, _, [Y]) :-
	can_jump(X, Y).

path_to_impl(X, Y, L, [Z|Path]) :-
	  can_jump(X, Z)
	, \+(member(Z, L))
	, \+(member(Y, L))
	, path_to_impl(Z, Y, [Z|L], Path).
