:-module(server,
	 [vdiff_warning/1,
	  debug_message/1,
	  start_react_server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:-ensure_loaded(user:testing).
:-ensure_loaded(user:react).
:-ensure_loaded(user:jsx).
:-ensure_loaded(user:vdiff).
:-ensure_loaded(user:dom).

user:term_expansion(end_of_file, _) :-
        prolog_load_context(module, Module),
        trigger_react_recompile(Module),
        fail.

start_react_server(Port):-
        http_server(http_dispatch, [port(Port)]).

:-multifile(react:goal_is_safe/1).
react:goal_is_safe(_).

vdiff_warning(X):- writeln(X).
debug_message(X):- writeln(X).