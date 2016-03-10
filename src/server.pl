:-module(server,
         [start_react_server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:-ensure_loaded(user:testing).
:-ensure_loaded(user:react).
:-ensure_loaded(user:jsx).
:-ensure_loaded(user:diff).
        
:-multifile
        user:end_of_file_hook/0.


user:term_expansion(end_of_file, _) :-
        prolog_load_context(module, Module),
        trigger_react_recompile(Module),
        fail.

start_react_server(Port):-
        http_server(http_dispatch, [port(Port)]).

:-multifile(react:goal_is_safe/1).
react:goal_is_safe(_).