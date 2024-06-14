:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

% URL handlers.
:- http_handler('/', home, []).
:- http_handler('/query', handle_query, []).

% Folder where files to retrieve are stored.
safe_folder('./scripts.js/').
:- http_handler('/query', handle_query, []).

handle_query(_Request) :-
    % Crear el diccionario con los datos
    Response = _{nombre: "ana", edad: 28},
    % Responder con el diccionario en formato JSON
    reply_json_dict(Response).

prolog_query(Query, Result) :-
    % Aquí puedes definir cómo quieres manejar las consultas
    % Por ejemplo, puedes evaluar la consulta utilizando el predicado call/1
    (   catch(call(Query), _, fail)
    ->  Result = 'true'
    ;   Result = 'false').

% Hechos y reglas de ejemplo
parent(john, mary).
parent(mary, susan).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

    
% Start the server on a specific port.
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- initialization
    format('*** Starting Server ***~n', []),
    (current_prolog_flag(argv, [SPort | _]) -> true ; SPort='8000'),
    atom_number(SPort, Port),
    format('*** Serving on port ~d *** ~n', [Port]),
    server(Port).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
