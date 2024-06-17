
:- module(heap, [
    enqueue_board/4,
	dequeue_board/4,
	empty_priority_queue/1,
	is_heap_empty/1
])
.
enqueue_board(HeapIn, IdBoard, Move, HeapOut):-
    priority_calculation(IdBoard, P),
    add_to_heap(HeapIn, P, [IdBoard, Move], HeapOut)
.
dequeue_board(HeapIn, IdBoard, Move, HeapOut):-
    get_from_heap(HeapIn, _, [IdBoard, Move], HeapOut)
.
empty_priority_queue(Heap) :-
    empty_heap(Heap).

is_heap_empty(Heap) :-
    heap_size(Heap, 0).