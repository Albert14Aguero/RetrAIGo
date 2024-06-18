:- module(inversion, [
    is_solvable/2
])
.
count_inversions([], 0).
count_inversions([_], 0).
count_inversions([H1| Tail], InvCount) :-
    count_inversions(Tail, RestInvCount),
    find_inversions(H1, Tail, R),
    InvCount is  R + RestInvCount
.
find_inversions(H1, [H2|Tail], Result) :-
    H1 \= empty,
	H2 \= empty,
	H1 > H2,
    find_inversions(H1, Tail, Result1),
    Result is Result1 + 1
.
find_inversions(H1, [_|Tail], Result) :-
    find_inversions(H1, Tail, Result)
.
find_inversions(_, _, 0).


is_solvable(Puzzle, Solution) :-
    findall(Elem, (member(Row, Puzzle), member(Elem, Row)), PuzzleFlat),
    findall(Elem, (member(Row, Solution), member(Elem, Row)), SolutionFlat),
    count_inversions(PuzzleFlat, PuzzleInvCount),
    count_inversions(SolutionFlat, SolutionInvCount),
    !,
    revision(PuzzleInvCount, SolutionInvCount)
.

revision(PuzzleInvCount, SolutionInvCount):-
    0 is PuzzleInvCount mod 2,
    0 is SolutionInvCount mod 2
.

revision(PuzzleInvCount, SolutionInvCount):-
    1 is PuzzleInvCount mod 2,
    1 is SolutionInvCount mod 2
.