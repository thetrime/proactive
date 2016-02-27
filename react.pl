:-module(react,
         [trigger_react_recompile/1]).

:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).


:-http_handler(root('react/component'), serve_react, [prefix]).
:-http_handler(root('react/listen'), notify_react, []).
:-http_handler(root('react/goal'), execute_react, []).

:-initialization(message_queue_create(react_reload_queue), restore).


serve_react(Request):-
        memberchk(path(Path), Request),
        atomic_list_concat(['', react, component, Module], '/', Path),        
        findall(Clause,
                react_clause(Module, Clause),               
                Clauses),
        format(current_output, 'Content-Type: text/prolog~n~n', []),
        forall(member(Clause, Clauses),
               ( numbervars(Clause, 0, _, [singletons(true)]),
                 write_term(current_output, Clause, [numbervars(true), quoted(true)]),
                 writeln(current_output, '.')
               )).

react_clause(Module, :-module(Module, Exports)):-
        module_property(Module, exports(Exports)).
react_clause(Module, Head:-Body):-
        current_predicate(_, Module:Head),
        predicate_property(Module:Head, interpreted),
        \+predicate_property(Module:Head, imported_from(_)),
        clause(Module:Head, Body, _).

react_clause(Module, SubmoduleClause):-
        current_predicate(_, Module:require(_)),
        predicate_property(Module:require(_), interpreted),
        \+predicate_property(Module:require(_), imported_from(_)),
        clause(Module:require(SubModule), _, _),
        react_clause(SubModule, SubmoduleClause).


notify_react(Request):-
        http_upgrade_to_websocket(notify_react_loop, [], Request).

notify_react_loop(Websocket):-
        thread_self(Self),                
        setup_call_cleanup(assert(react_listener(Self)),
                           notify_react_loop_1(Websocket),
                           retractall(react_listener(Self))).

notify_react_loop_1(Websocket):-
        thread_get_message(Message),
        ws_send(Websocket, text(Message)),
        !,
        notify_react_loop_1(Websocket).

:-dynamic(react_listener/1).

execute_react(Request):-
        http_upgrade_to_websocket(execute_react_ws, [], Request).

execute_react_ws(WebSocket):-
        ws_receive(WebSocket, Message, []),
        Message.opcode == text,
        Data = Message.data,
        read_term_from_atom(Data, Goal, []),
        execute_react_ws(WebSocket, Goal, Goal).

check:string_predicate(react:check_data/1).
check_data(";").

:-meta_predicate(execute_react_ws(+, +, 0)).
:-multifile(check:string_predicate/1).
execute_react_ws(WebSocket, ReplyGoal, Goal):-
        ws_receive(WebSocket, Message, []),
        Message.opcode == text,
        Data = Message.data,
        check_data(Data),
        execute_react_goal(Goal, ReplyGoal, WebSocket).

:-meta_predicate(execute_react_goal(0, +, +)).
execute_react_goal(Goal, ReplyGoal, WebSocket):-
        setup_call_catcher_cleanup(true,
                                   Goal,
                                   Catcher,
                                   react_cleanup(ReplyGoal, Catcher, WebSocket)),
        ( var(Catcher)->            
            send_reply(WebSocket, exit(ReplyGoal))
        ; otherwise->
            true
        ),
        % Wait for the next request
        ws_receive(WebSocket, Message, []),
        Data = Message.data,
        \+(( Message.opcode == text,
             check_data(Data))),
        !.
        

react_cleanup(Goal, exit, WebSocket):-
        send_reply(WebSocket, cut(Goal)).

react_cleanup(_Goal, exception(E), WebSocket):-
        send_reply(WebSocket, exception(E)).

react_cleanup(_Goal, external_exception(E), WebSocket):-
        send_reply(WebSocket, exception(E)).

react_cleanup(_Goal, fail, WebSocket):-
        send_reply(WebSocket, fail).

react_cleanup(Goal, !, WebSocket):-
        send_reply(WebSocket, cut(Goal)).

send_reply(WebSocket, Term):-
        format(atom(Text), '~q', [Term]),
        ws_send(WebSocket, text(Text)).

trigger_react_recompile(Module):-
        forall(react_listener(Queue),
               thread_send_message(Queue, Module)).

:-meta_predicate(user:on_server(0)).
user:on_server(X):- X.
