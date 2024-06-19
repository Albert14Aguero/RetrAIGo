:-consult(main).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).

% URL handlers.
:- http_handler('/solve', handle_solve, []).
:- http_handler('/exaple', handle_exaple, []).

handle_exaple(_Request) :-
    DictOut = _{moves: 0, quantity:0},
    reply_json_dict(DictOut)
.
handle_solve(Request) :-
    http_read_json_dict(Request, DictIn),
    Initial = DictIn.initial,
    Goal = DictIn.goal,
    Distance = DictIn.distance,
    transformar_lista_de_listas(Initial, L),
    transformar_lista_de_listas(Goal, G),
    solve(L, G, Distance, Ex, Result),
    ( Result = -1
        ->
        (DictOut = _{error: Ex},
        reply_json_dict(DictOut));
        (contar_elementos(Result, N),
        DictOut = _{moves: Result, quantity:N, expantion:Ex},
        reply_json_dict(DictOut))
    )
.
getSwiplPort('8000'). 
getNextPort('http://localhost:3000').

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- initialization
    format('* Starting Server *~n', []),
    (current_prolog_flag(argv, [SPort | _]) -> true ; getSwiplPort(SPort)),
    atom_number(SPort, Port),
    format('* Serving on port ~d * ~n', [Port]),
    getNextPort(AllowedPort),
    set_setting_default(http:cors, [AllowedPort]), % antes estaba como * (cambiar devuelta si no sirve)
    server(Port).