:- module(diccionary, [
    inicializar_diccionario/0,
	guardar_en_diccionario/2,
	obtener_de_diccionario/2,
	eliminar_diccionario/0
])
.
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
eliminar_diccionario:-
    nb_delete(diccionario_global)
.

