:- module(inversion, [
    is_solvable/2
])
.
count_inversions([], 0).
count_inversions([_], 0).
count_inversions([H|T], InvCount) :-
    count_inversions(T, RestCount),
    find_inversions(T, H, Invs),
    InvCount is RestCount + Invs
.
find_inversions([], _, 0).
find_inversions([H|_], Elem, 1) :-
    H \= empty,
	Elem \= empty,
	H > Elem
.
find_inversions(_, _, 0).
is_solvable(Puzzle, Solution) :-
    findall(Elem, (member(Row, Puzzle), member(Elem, Row)), PuzzleFlat),
    findall(Elem, (member(Row, Solution), member(Elem, Row)), SolutionFlat),
    count_inversions(PuzzleFlat, PuzzleInvCount),
    count_inversions(SolutionFlat, SolutionInvCount),
    0 is PuzzleInvCount mod 2,
    0 is SolutionInvCount mod 2
.