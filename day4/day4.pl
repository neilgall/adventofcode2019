
digit(0).
digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).

in_range(A, B, C, D, E, F) :-
	 X = (((((A * 10 + B) * 10 + C) * 10) + D) * 10 + E) * 10 + F
   , 272091 =< X
   , X =< 815432.

has_adjacent_digits_the_same(A, B, C, D, E, F) :-
	  A == B
	; B == C
	; C == D
	; D == E
	; E == F.

adjacent_digits_not_in_larger_group(A, B, C, D, E, F) :-
              A == B, B \= C
    ; A \= B, B == C, C \= D
    ; B \= D, C == D, D \= E
    ; C \= E, D == E, E \= F
    ; D \= E, E == F.

has_no_decreasing_digits(A, B, C, D, E, F) :-
	  A =< B
	, B =< C
	, C =< D
	, D =< E
	, E =< F.

solve_part1(A, B, C, D, E, F) :-
	  digit(A)
	, digit(B)
	, digit(C)
	, digit(D)
	, digit(E)
	, digit(F)
	, has_adjacent_digits_the_same(A, B, C, D, E, F)
	, has_no_decreasing_digits(A, B, C, D, E, F)
	, in_range(A, B, C, D, E, F).

solve_part2(A, B, C, D, E, F) :-
	  solve_part1(A, B, C, D, E, F)
	, adjacent_digits_not_in_larger_group(A, B, C, D, E, F).

part1(Count) :-
	  setof([A, B, C, D, E, F], solve_part1(A, B, C, D, E, F), Set)
	, length(Set, Count).

part2(Count) :-
	  setof([A, B, C, D, E, F], solve_part2(A, B, C, D, E, F), Set)
	, length(Set, Count).
