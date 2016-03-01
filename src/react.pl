:-module(react,
         [raise_event/2,
          wait_for/1,         
          trigger_react_recompile/1]).

%       You MUST provide an implementation of react:goal_is_safe/1 or on_server/1 will always fail on the client.
%       This is because an unscrupulous user could easily execute on_server with whatever goal they want! 


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
        findall(Candidate,
                related_modules(Module, Candidate),
                Modules),
        sort(Modules, ModulesWithoutDuplicates),
        findall(Clause,
                ( member(AModule, ModulesWithoutDuplicates),
                  react_clause(AModule, Clause)
                ),
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

related_modules(Root, Root).
related_modules(Module, Related):-
        current_predicate(_, Module:requires(_)),
        predicate_property(Module:requires(_), interpreted),
        \+predicate_property(Module:requires(_), imported_from(_)),
        clause(Module:requires(SubModule), _, _),
        related_modules(SubModule, Related).



notify_react(Request):-
        http_upgrade_to_websocket(notify_react_loop, [], Request).

notify_react_loop(Websocket):-
        thread_self(Self),
        thread_create(ws_notify_slave(Websocket, Self), Slave, [detached(false)]),
        setup_call_cleanup(assert(react_listener(Self)),
                           notify_react_loop_1(Websocket, Slave),
                           retractall(react_listener(Self))).

ws_notify_slave(Websocket, Owner):-
        ws_receive(Websocket, Message),
        ( Message.opcode == close->
            thread_send_message(Owner, close)
        ; otherwise->
            ws_notify_slave(Websocket, Owner)
        ).

notify_react_loop_1(Websocket, Slave):-
        thread_get_message(Message),
        ( Message == close->
            thread_join(Slave, _),
            ws_close(Websocket, 1000, goodbye),
            throw(terminated)
        ; Message = text(Data)->
            ws_send(Websocket, text(Data))
        ; otherwise->
            format(user_error, 'Unexpected message: ~q~n', [Message])
        ),
        !,
        notify_react_loop_1(Websocket, Slave).

:-dynamic(react_listener/1).

execute_react(Request):-
        http_upgrade_to_websocket(execute_react_ws, [], Request).

:-multifile(react:goal_is_safe/1).
execute_react_ws(WebSocket):-
        ws_receive(WebSocket, Message, []),
        ( Message.opcode == text->
            Data = Message.data,
            read_term_from_atom(Data, Goal, []),
            ( goal_is_safe(Goal)->
                execute_react_ws(WebSocket, Goal, Goal)
            ; permission_error(execute, goal, Goal)
            )
        ; Message.opcode == close->
            !
        ).

check:string_predicate(react:check_data/1).
check_data(";").

:-meta_predicate(execute_react_ws(+, +, 0)).
:-multifile(check:string_predicate/1).
execute_react_ws(WebSocket, ReplyGoal, Goal):-
        ws_receive(WebSocket, Message, []),
        ( Message.opcode == text->
            Data = Message.data,
            check_data(Data),
            execute_react_goal(Goal, ReplyGoal, WebSocket)
        ; Message.opcode == close->
            !
        ).

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
        ( Message.opcode == text->
            \+check_data(Data)
        ; Message.opcode == close->
            true
        ),
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
               thread_send_message(Queue, text(Module))).
              

:-meta_predicate(user:on_server(0)).
user:on_server(X):- X.

raise_event(Key, _):- permission_error(raise, server_side_event, Key).
wait_for(List):- permission_error(wait_for, server_side_event, List).