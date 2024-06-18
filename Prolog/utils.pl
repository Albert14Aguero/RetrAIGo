
:- module(utils, [
    zip/3,
	enumerate/2,
	index_of/3,
	new_id/2,
    swap/6,
    board_from/2,
    board_list/2,
    board_to/2,
	transformar_lista_de_listas/2,
	contar_elementos/2
])
.

zip(L, M, Z) :- maplist( [X, Y, [X, Y]] >> true, L, M, Z)
.
enumerate(L, EL) :-
    length(L, N),  numlist(1, N, LN), zip(LN, L, EL)
.
index_of(V, L, P) :- nth1(P, L, V).

new_id(B, I) :- gensym(B, I).

swap(L, L, A, B, Result, Result):-
	A =< B,
	nth1(B, L, Y, L0),
    nth1(A, L0, X, L1),
    nth1(A, L2, Y, L1),
    nth1(B, Result, X, L2), !
.
swap(L, L, A, B, Result, Result):-
	B < A,
	nth1(A, L, Y, L0),
    nth1(B, L0, X, L1),
    nth1(B, L2, Y, L1),
    nth1(A, Result, X, L2), !
.
swap(L1, L2, Position, Position2, R3, R4):-
	Position =< Position2,
	nth1(Position, L1, E1, R1),
	nth1(Position2, L2, E2, R2),
	nth1(Position, R3, E2, R1),
	nth1(Position2, R4, E1, R2)
.
swap(L1, L2, Position, Position2, R3, R4):-
	Position2 < Position,
	nth1(Position2, L1, E1, R1),
	nth1(Position, L2, E2, R2),
	nth1(Position2, R3, E2, R1),
	nth1(Position, R4, E1, R2)
.

board_from(L, Id) :-
    board_new_id(Id),
    board_clear(Id),
    enumerate(L, EL),
    forall(member(R, EL), board_add(Id, R))
.


board_list(Id, ListasOrdenadas) :-
    findall(N-List, board_row(Id, N, List), Pares),
    keysort(Pares, ParesOrdenados),
    pairs_values(ParesOrdenados, ListasOrdenadas)
.
board_to(Id, LS) :-
	findall([I, Row], board_row(Id, I, Row), L),
	sort(L, LS)
.

% Caso base: la lista vacía se transforma en sí misma
transformar_lista([], []).

transformar_lista([0|Cola], ['empty'|ColaTransformada]) :-
    transformar_lista(Cola, ColaTransformada).

transformar_lista([Cabeza|Cola], [Cabeza|ColaTransformada]) :-
    Cabeza \= 0,
    transformar_lista(Cola, ColaTransformada)
.

transformar_lista_de_listas([], []).
transformar_lista_de_listas([Lista|ColaDeListas], [ListaTransformada|ColaDeListasTransformada]) :-
    transformar_lista(Lista, ListaTransformada),
    transformar_lista_de_listas(ColaDeListas, ColaDeListasTransformada)
.

contar_elementos([], 0).
contar_elementos([_|Cola], N) :-
    contar_elementos(Cola, NCola),
    N is NCola + 1.
