:- use_module(library(heaps)).
:-use_module(utils).
:-use_module(priority).
:-use_module(diccionary).
:-use_module(inversion).
:-use_module(heap).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lista_de_movimientos(ListIn, Id, ListOut):-
     obtener_de_diccionario(Id, IdR, Move, Priority, _),
     append(ListIn, [[Move, Priority]], List),
     lista_de_movimientos(List, IdR, ListOut)
     
.
lista_de_movimientos(ListIn, _, ListIn).

invertir_lista([], []).
invertir_lista([Cabeza|Cola], ListaInvertida) :-
    invertir_lista(Cola, ColaInvertida),
    append(ColaInvertida, [Cabeza], ListaInvertida).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-dynamic expantionList/1.

expantion_clear:-
    retractall(expantionList(_))
.
set_expantion(E):-
    assert(expantionList(E))
.
update_expantion(L):-
    expantionList(L1),
    append(L1, [L], L2),
    expantion_clear,
    set_expantion(L2)
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-dynamic distance/1.
distance_clear:-
    retractall(distance(_))
.
set_distance(D):-
    assert(distance(D))
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
   retractall(board_empty(_, _, _)),
   reset_gensym('board_')
.
   
board_clear(Id) :-
   retractall(board_row(Id, _, _)),
   retractall(board_empty(Id,_)),
   retractall(visited(_))
.

board_clone(Id, IdC):-
   findall([RId, R], board_row(Id, RId, R), EL),
   board_new_id(IdC),
   forall(member(RC, EL), board_add(IdC, RC))
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
	assert(visited(HI))
.

board_start_play_dsf(Id, R) :-
    empty_priority_queue(EmptyHeap),
    distance(D),
    priority_calculation(Id, P, D),
    enqueue_board(EmptyHeap, Id, 'root', P, InitialHeap),
	board_play_dfs(InitialHeap, R)
.

board_check(Id):-
	board_to(Id, L),
	obtener_de_diccionario(resultado, IdG, _M, _P, _),
	board_to(IdG, G),
	L = G
.
board_play_dfs(Heap, R) :-
    \+ is_heap_empty(Heap),
    dequeue_board(Heap, Id, _, _),
    board_check(Id),
    lista_de_movimientos([], Id, L),
    invertir_lista(L, R),
    update_expantion('***************************************'),
    update_expantion('***************************************'),
    update_expantion('***************************************')
.

board_play_dfs(Heap, R) :-
    \+ is_heap_empty(Heap),
    dequeue_board(Heap, Id, Move, RestHeap),
    distance(D),
    priority_calculation(Id, Pri, D),
    \+ board_is_visited(Id),
    board_set_visited(Id),
    update_expantion('***************************************'),
    multiple_atom_concat('(1)Trying board:', [Id, Move, 'Priority:', Pri], RA),
    update_expantion(RA),
    findall([IdChild, DChild], board_child(Id, IdChild, DChild), ChildList),
    enqueue_children(ChildList, RestHeap, NewHeap, Id),
    board_play_dfs(NewHeap, R)
.

board_play_dfs(Heap, R):-
    \+ is_heap_empty(Heap),
    dequeue_board(Heap, Id, D, NewHeap),
    board_is_visited(Id),
    update_expantion('***************************************'),
    multiple_atom_concat('(2)Visited board:', [Id, ' move =',D], RA2),
    update_expantion(RA2),
    board_play_dfs(NewHeap, R)
.


enqueue_children([], Heap, Heap, _).
enqueue_children([[IdC, DC] | Rest], Heap, NewHeap, IdF):-
    distance(D),
    priority_calculation(IdC, P, D),
    obtener_de_diccionario(IdF, _, _, _, A1),
    A2 is A1 + 1,
    guardar_en_diccionario(IdC, IdF, DC, P, A2),
    update_expantion('---------------------------------------'),
    multiple_atom_concat('Enqueue:', [IdC, 'Move:', DC], ResultAtom),
    update_expantion(ResultAtom),
    multiple_atom_concat('Priority:', [P, 'Height:', A2], Atom2),
    update_expantion(Atom2),
    enqueue_board(Heap, IdC, DC, P, TempHeap),
    enqueue_children(Rest, TempHeap, NewHeap, IdF)
.
solve(L, G, _Distance, 'No se pudo resolver el problema', -1):-
    \+is_solvable(L, G)
.
solve(L, G, Distance, Ex, R) :-
    board_clear_all,
	eliminar_diccionario,
    expantion_clear,
    set_expantion([]),
    distance_clear,
    set_distance(Distance),
	inicializar_diccionario,
	board_clear(_),
    is_solvable(L, G),
	board_from(L, Id),
    board_from(G, IdG),
    guardar_en_diccionario(Id, inicio, root, 0, 1),
	guardar_en_diccionario(resultado, IdG, final, 0, _),
    board_start_play_dsf(Id, R),
    expantionList(Ex)
.

test(R):-
	L = [
        [1,3,empty],
        [8,2,4],
        [7,6,5]],
	G = [
        [1,2,3],
        [8,empty,4],
        [7,6,5]],
    solve(L, G, R)

.
