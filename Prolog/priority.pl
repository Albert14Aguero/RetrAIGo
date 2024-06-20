:- module(priority, [
	priority_calculation/3
])
.
priority_calculation(Id, P, 1):-
    board_list(Id, L1),
    obtener_de_diccionario('resultado', IdR, _, _, _), 
    board_list(IdR, L2),
    manhattan_distance(L1, L2, P)
.
priority_calculation(Id, P, 0):-
    board_list(Id, L1),
    obtener_de_diccionario('resultado', IdR, _, _, _), 
    board_list(IdR, L2),
    difference_distance(L1, L2, P)
.

find_position(Matrix, Number, Row, Col) :-
    nth1(Row, Matrix, RowList),
    nth1(Col, RowList, Number), !.


manhattan_distance(List1, List2, Result):-
	findall(R, (between(1, 8, X), manhattan_calculation(List1, List2, X, R)), ResultList),
    sumlist(ResultList, Result)
.

manhattan_calculation(List1, List2, Number, Result):-
	find_position(List1, Number, Row1, Col1),
	find_position(List2, Number, Row2, Col2),
	X is Row1 - Row2,
	Y is Col1 - Col2,
	abs(X, R1),
	abs(Y, R2),
 	Result is R1 + R2
.

difference_distance(List1, List2, Result):-
	list_of_lists_difference(List1, List2, Result)
.

list_difference([], [], 0).
list_difference([], [_|_], 1).
list_difference([_|_], [], 1).

list_difference([H1|T1], [H2|T2], Count) :-
    H1 == H2,
    list_difference(T1, T2, CountRest),
    Count is CountRest
.

list_difference([H1|T1], [H2|T2], Count) :-
    H1 \== H2,
    list_difference(T1, T2, CountRest),
    Count is CountRest + 1
.

list_of_lists_difference([], [], 0).
list_of_lists_difference([L1|T1], [L2|T2], Count) :-
    list_difference(L1, L2, Diff),
    list_of_lists_difference(T1, T2, DiffRest),
    Count is Diff + DiffRest
.
list_of_lists_difference([L1|T1], [L2|T2], Count) :-
    list_difference(L1, L2, Diff),
    list_of_lists_difference(T1, T2, DiffRest),  
    Count is Diff + DiffRest
.