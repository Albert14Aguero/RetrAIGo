:- module(diccionary, [
    inicializar_diccionario/0,
	guardar_en_diccionario/5,
	obtener_de_diccionario/5,
	eliminar_diccionario/0
])
.
inicializar_diccionario :-
    nb_setval(diccionario_global, _{})
.

guardar_en_diccionario(IdC, IdBoard, Move, Priority, Altura) :-
    nb_getval(diccionario_global, DiccionarioActual),
    put_dict(IdC, DiccionarioActual, [IdBoard, Move, Priority, Altura], NuevoDiccionario),
    nb_setval(diccionario_global, NuevoDiccionario)
.

obtener_de_diccionario(IdC, IdBoard, Move, Priority, Altura) :-
    nb_getval(diccionario_global, DiccionarioActual),
    get_dict(IdC, DiccionarioActual, [IdBoard, Move, Priority, Altura])
.
eliminar_diccionario:-
    nb_delete(diccionario_global)
.

