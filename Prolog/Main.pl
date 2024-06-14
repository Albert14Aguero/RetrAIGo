:- use_module(library(heaps)).

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

board_from(L, Id) :-
    board_new_id(Id),
    board_clear(Id),
    enumerate(L, EL),
    forall(member(R, EL), board_add(Id, R))
.
board_list(Id, L):-
    findall(Row, board_row(Id, _, Row), L)
.
board_to(Id, LS) :-
	findall([I, Row], board_row(Id, I, Row), L),
	sort(L, LS)
.

%%%%%%%%%%%%%% Calculations %%%%%%%%%%%%%%%%%

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
priority_calculation(Id, P):-
    board_list(Id, L1),
    obtener_de_diccionario('resultado', IdR), 
    board_list(IdR, L2),
    manhattan_distance(L1, L2, P)
.
enqueue_board(HeapIn, IdBoard, Move, HeapOut):-
    priority_calculation(IdBoard, P),
    add_to_heap(HeapIn, P, [IdBoard, Move], HeapOut)
.
dequeue_board(HeapIn, IdBoard, Move, HeapOut):-
    get_from_heap(HeapIn, _, [IdBoard, Move], HeapOut)
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
empty_priority_queue(Heap) :-
    empty_heap(Heap).

is_heap_empty(Heap) :-
    heap_size(Heap, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    empty_priority_queue(EmptyHeap),
    enqueue_board(EmptyHeap, Id, 'root', InitialHeap),
	board_play_dfs(InitialHeap)
.

board_check(Id):-
	board_to(Id, L),
	obtener_de_diccionario(resultado, IdG),
	board_to(IdG, G),
	L = G
.
board_play_dfs(Heap) :-
    \+ is_heap_empty(Heap),
    dequeue_board(Heap, Id, _, _),
    board_check(Id),
    format('~nEntontrado ~q!!!!! ~n', [Id]),
    board_show(Id)
.

board_play_dfs(Heap) :-
    \+ is_heap_empty(Heap),
    dequeue_board(Heap, Id, Move, RestHeap),
    priority_calculation(Id, Pri),
    format('~n(1)Priority ~q ~n', [Pri]),
    format('~n(1)Trying board ~a move=~s~n', [Id, Move]),
    \+ board_is_visited(Id),
   board_set_visited(Id),
   findall([IdChild, DChild], board_child(Id, IdChild, DChild), ChildList),
   enqueue_children(ChildList, RestHeap, NewHeap),
   board_play_dfs(NewHeap)
.

board_play_dfs(Heap):-
    \+ is_heap_empty(Heap),
    dequeue_board(Heap, Id, D, NewHeap),
    format('~n(2)Trying board ~a move=~s~n', [Id, D]),
    board_is_visited(Id),
    format('~n(2)Visited board ~a move=~s~n', [Id, D]),
    board_play_dfs(NewHeap)
.


enqueue_children([], Heap, Heap).
enqueue_children([[IdC, DC] | Rest], Heap, NewHeap):-
    enqueue_board(Heap, IdC, DC, TempHeap),
    enqueue_children(Rest, TempHeap, NewHeap)
.
test(Id) :-
    board_clear_all,
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
	board_clear(_),
	board_from(L, Id),
	board_show(Id),
    board_from(G, IdG),
	guardar_en_diccionario(resultado, IdG)
.

:-
	test(I),
	board_start_play_dsf(I)
.
