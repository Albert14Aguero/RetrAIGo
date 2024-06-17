:- use_module(library(heaps)).
:-use_module(utils).
:-use_module(priority).
:-use_module(diccionary).
:-use_module(inversion).
:-use_module(heap).


:- dynamic visited/1.
visited_clear :-
	retractall(visited(_))
.

set_visited(I) :-
    
	assert(visited(I))
.
:- dynamic board_row/3.
:- dynamic board_empty/3.

board_new_id(I) :- new_id('board_', I).

board_clear_all :-
   retractall(board_row(_, _, _)),
   retractall(board_empty(_, _, _))
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
    retract(board_row(Id, I, _)),
	board_add(Id, [I, RS])
.
board_add_empty(Id, I, J) :-
    retractall(board_empty(Id, _,_)),
    assert(board_empty(Id, I, J))
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
    board_check(Id)
.

board_play_dfs(Heap) :-
    \+ is_heap_empty(Heap),
    dequeue_board(Heap, Id, _Move, RestHeap),
    \+ board_is_visited(Id),
   board_set_visited(Id),
   findall([IdChild, DChild], board_child(Id, IdChild, DChild), ChildList),
   enqueue_children(ChildList, RestHeap, NewHeap),
   board_play_dfs(NewHeap)
.

board_play_dfs(Heap):-
    \+ is_heap_empty(Heap),
    dequeue_board(Heap, Id, _, NewHeap),
    board_is_visited(Id),
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
    is_solvable(L, G),
	board_from(L, Id),
    board_from(G, IdG),
	guardar_en_diccionario(resultado, IdG),
    board_start_play_dsf(Id)
.

