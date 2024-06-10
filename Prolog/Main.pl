/*
	[[1,2,e],
	[8,2,4],
	[7,6,4]]

--board_from-->

bord_row(1, [1,3,e]).
bord_row(2, [8,2,4]).
bord_row(3, [7,6,5]).

predicados dinamicos
*/
%%%%%%%%%%%%%%%%% UTILS %%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic visited/1.
visited_clear :-
	retractall(visited(_))
.

set_visited(I) :-
    
	assert(visited(I))
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inicializar_diccionario :-
    nb_setval(diccionario_global, _{})
.

guardar_en_diccionario(Hash, IdBoard) :-
    nb_getval(diccionario_global, DiccionarioActual),
    put_dict(Hash, DiccionarioActual, IdBoard, NuevoDiccionario),
    nb_setval(diccionario_global, NuevoDiccionario)
.

obtener_de_diccionario(Hash, IdBoard) :-
    nb_getval(diccionario_global, DiccionarioActual),
    get_dict(Hash, DiccionarioActual, IdBoard)
.
eliminar_diccionario :-
    nb_delete(diccionario_global)
.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
board_from(L, Id) :-
    board_new_id(Id),
    board_clear(Id),
    enumerate(L, EL),
    forall(member(R, EL), board_add(Id, R))
.

board_to(Id, LS) :-
	findall([I, Row], board_row(Id, I, Row), L),
	sort(L, LS)
.

:- dynamic board_row/3.
:- dynamic board_empty/3.

board_new_id(I) :- new_id('board_', I).

board_clear_all :-
   retractall(board_row(_, _)),
   retractall(board_empty(_,_))
.
   
board_clear(Id) :-
   retractall(board_row(Id, _)),
   retractall(board_empty(Id,_)),
   retractall(visited(_))
.

board_clone(Id, IdC):-
   findall([RId, R], board_row(Id, RId, R), EL),
   board_new_id(IdC),
   forall(member(RC, EL), board_add(IdC, RC))
.

board_add(Id, [I, R]) :-
    assert(board_row(Id, I, R)),
    ( index_of(empty, R, J) -> board_add_empty(Id, I, J) ; true )
.
board_update(Id, I, RS) :-
    writeln(['*** update ***', Id, I, RS]),
    retract(board_row(Id, I, _)),
	board_add(Id, [I, RS])
.
board_add_empty(Id, I, J) :-
    retractall(board_empty(Id, _,_)),
    assert(board_empty(Id, I, J))
.

board_show(Id) :- 
   writeln('Board rows:'),
   findall([IR, Row], board_row(Id, IR, Row), LR),
   sort(LR, LRS),
   forall( member([I, R], LRS), writeln([I, R]) ),
   writeln('Empty at:'),
   board_empty(Id, EI, EJ),
   write([EI, EJ])
.

board_get_valid_move(Id, P, D) :-
    board_empty(Id, I, J),
    ( (I > 1, I1 is I - 1, P=[I1, J], D = up);
      (I < 3, I1 is I + 1, P=[I1, J], D = down);
      (J > 1, J1 is J - 1, P=[I, J1], D = left);
      (J < 3, J1 is J + 1, P=[I, J1], D = right))
.

board_apply_move(NewId, Move) :-
	board_get_valid_move(NewId, [Fila, Columna], Move),
	board_row(NewId, Fila, List),
	board_empty(NewId, FilaEpty, ColumnaEmpty),
	board_row(NewId, FilaEpty, ListEmpty),
	swap(List, ListEmpty, Columna, ColumnaEmpty, I, R),
	board_update(NewId, Fila, I),
	board_update(NewId, FilaEpty, R)
.
board_child(Id, IdChild, D) :-
   member(D, [left, right, up, down]),
   board_clone(Id, IdChild), 
   board_apply_move(IdChild, D)
.

board_is_visited(I) :-
	board_to(I, L),
	term_hash(L, HI),
	visited(HI)
.
board_set_visited(I) :-
    board_to(I, L),
    term_hash(L, HI),
	assert(visited(HI)),
	guardar_en_diccionario(HI, I)
.

board_start_play_dsf(Id) :-
    Depth = 4,
	D = root,
	board_play_dfs(Id, Depth, D)
.

board_check(Id):-
	board_to(Id, L),
	obtener_de_diccionario(resultado, IdG),
	board_to(IdG, G),
	L = G
.
board_play_dfs(Id, Depth, _) :-
   board_check(Id),
   format('~nEntontrado en ~q!!!!! ~n', [Depth]),
   board_show(Id)
.

board_play_dfs(Id, Depth, D) :-
   format('~n(1)Trying board ~a depth=~d move=~s~n', [Id, Depth, D]),
   Depth > 0, 
   \+ board_is_visited(Id),
   board_set_visited(Id),
   findall([IdChild, DChild], board_child(Id, IdChild, DChild), ChildList),
   Depth1 is Depth - 1,
   forall(member([IdC, DC], ChildList), board_play_dfs(IdC, Depth1, DC)),
   format('~n(1)Processed board ~a depth=~d move=~s~n', [Id, Depth, D])
.
board_play_dfs(Id, Depth, D):-
   format('~n(2)Trying board ~a depth=~d move=~s~n', [Id, Depth, D]),
   board_is_visited(Id),
   format('~n(2)Visited board ~a depth=~d move=~s~n', [Id, Depth, D])
.
board_play_dfs(Id, Depth, D):-
   format('~n(3)Trying board ~a depth=~d move=~s~n', [Id, Depth, D]),
   Depth = 0,
   format('~n(3)Depth exceding board ~a depth=~d move=~s~n', [Id, Depth, D])
.
   
test(Id) :-
	eliminar_diccionario,
	inicializar_diccionario,
	L = [
        [1,3,empty],
        [8,2,4],
        [7,6,5]],
	G = [
        [1,2,3],
        [8,empty,4],
        [7,6,5]],
	board_from(G, IdG),
	guardar_en_diccionario(resultado, IdG),
	board_clear(_),
	board_from(L, Id),
	board_show(Id)
.

:-
	test(I),
	board_start_play_dsf(I)
.
