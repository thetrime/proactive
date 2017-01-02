:-module(react,
         [raise_event/2,
          wait_for/1,
          bubble_event/3,
          trigger_react_recompile/1,
          vdiff_warning/1,
          jsx/2]).

%       You MUST provide an implementation of react:goal_is_safe/1 or on_server/1 will always fail on the client.
%       This is because an unscrupulous user could easily execute on_server with whatever goal they want!

%       If react:allow_access_to_form(+FormId) has any clauses, a form will only be served if it succeeds

%       If you want to handle the execution of the goal yourself, you can define a clause of react:react_goal_hook/2
%       If this has at least one clause defined, it will be passed any goals that on_server/1 is going to execute
%       Otherwise, they will be exeucted directly.

user:term_expansion(:-table_predicate(Module:Indicator), tabled_predicate(Module, Indicator)).
user:term_expansion(:-table_predicate(Indicator), tabled_predicate(user, Indicator)).

:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).


:-http_handler(root('react/component'), serve_react, [prefix]).
:-http_handler(root('react/listen'), notify_react, []).
:-http_handler(root('react/goal'), execute_react, []).

:-initialization(message_queue_create(react_reload_queue), restore).




serve_react(Request):-
        memberchk(path(Path), Request),
        atomic_list_concat(['', react, component, Module], '/', Path),
        ( predicate_property(react:allow_access_to_form(_), number_of_clauses(_))->
            ( allow_access_to_form(Module)->
                true
            ; otherwise->
                throw(http_reply(forbidden(Path)))
            )
        ; true
        ),
        ( current_module(Module)->
            findall(Candidate,
                    related_modules(Module, Candidate),
                    Modules),
            sort(Modules, ModulesWithoutDuplicates),
            findall(Clause,
                    ( member(AModule, ModulesWithoutDuplicates),
                      react_clause(AModule, Clause)
                    ),
                    Clauses),
            format(current_output, 'Access-Control-Allow-Origin: *~n', []),
            ( memberchk(accept_encoding(AcceptEncoding), Request),
              sub_atom(AcceptEncoding, _, _, _, deflate)->
                format(current_output, 'Content-Encoding: deflate~n', []),
                zopen(current_output, TargetStream, [])
            ; otherwise->
                TargetStream = current_output
            ),
            format(current_output, 'Content-Type: text/prolog~n~n', []),
            forall(member(Clause, Clauses),
                   ( numbervars(Clause, 0, _, [singletons(true)]),
                     write_term(TargetStream, Clause, [numbervars(true), quoted(true), ignore_ops(true)]),
                     writeln(TargetStream, '.')
                   )),
            ( TargetStream == current_output ->
                true
            ; otherwise->
                close(TargetStream)
            )
        ; otherwise->
            format(current_output, 'Access-Control-Allow-Origin: *~n', []),
            format(current_output, 'Content-Type: text/prolog~n~n', [])
        ).


react_clause(Module, :-module(Module, Exports)):-
        module_property(Module, exports(Exports)).
react_clause(Module, Head:-Body):-
        current_predicate(_, Module:Head),
        predicate_property(Module:Head, interpreted),
        \+predicate_property(Module:Head, imported_from(_)),
        clause(Module:Head, Body, _).
react_clause(Module, Head):-
        current_predicate(_, Module:tabled_predicate(_, _)),
        clause(Module:tabled_predicate(SourceModule, Name/Arity), true),
        functor(Head, Name, Arity),
        call(SourceModule:Head).


related_modules(Root, Root).
related_modules(Module, Related):-
        current_predicate(_, Module:depends_on(_)),
        predicate_property(Module:depends_on(_), interpreted),
        \+predicate_property(Module:depends_on(_), imported_from(_)),
        clause(Module:depends_on(SubModule), _, _),
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
        ( http_in_session(SessionID)->
            true
        ; SessionID = {null}
        ),
        http_upgrade_to_websocket(execute_react_ws_guarded(SessionID, Request), [], Request).

:-multifile(react:goal_is_safe/1).
:-multifile(react:allow_access_to_form/1).

% SWI uses message_to_string/2 to print the message if there is an error
% This is fine, bug RFC-6455 says that no control packet may be > 125 bytes, or fragmented
% and the error can easily be much longer than that. So we just suppress it here and use 'error' instead
execute_react_ws_guarded(SessionID, OriginalRequest, WebSocket):-
        ( SessionID == {null}->
            true
        ; b_setval(http_session_id, SessionID)
        ),
        ( catch(execute_react_ws(OriginalRequest, WebSocket), E, true)->
	    ( var(E)->
		Msg = bye, Code = 1000
	    ; otherwise->
		Msg = error, Code = 1011
	    )
	; Msg = failed, Code = 1011
	),
	catch(ws_close(WebSocket, Code, text(Msg)), Error, print_message(error, Error)).

execute_react_ws(OriginalRequest, WebSocket):-
        ws_receive(WebSocket, Message, []),
        ( Message.opcode == text->
            Data = Message.data,
            read_term_from_atom(Data, Goal, []),
            ( goal_is_safe(Goal)->
                execute_react_ws(OriginalRequest, WebSocket, Goal, Goal)
            ; permission_error(execute, goal, Goal)
            )
        ; Message.opcode == close->
            !
        ).

check:string_predicate(react:check_data/1).
check_data(";").

:-meta_predicate(execute_react_ws(+, +, +, 0)).
:-multifile(check:string_predicate/1).
execute_react_ws(OriginalRequest, WebSocket, ReplyGoal, Goal):-
        ws_receive(WebSocket, Message, []),
        ( Message.opcode == text->
            Data = Message.data,
            check_data(Data),
            execute_react_ws_1(OriginalRequest, Goal, ReplyGoal, WebSocket)
        ; Message.opcode == close->
            !
        ).

:-meta_predicate(execute_react_ws_1(+, 0, +, +)).
execute_react_ws_1(OriginalRequest, Goal, ReplyGoal, WebSocket):-
	setup_call_catcher_cleanup(true,
                                   execute_react_goal(OriginalRequest, Goal),
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

:-multifile(react:react_goal_hook/2).
:-meta_predicate(react:react_goal_hook(+, 0)).

:-meta_predicate(execute_react_goal(+, 0)).
execute_react_goal(OriginalRequest, Goal):-
        ( predicate_property(react:react_goal_hook(_, _), number_of_clauses(_))->
            react_goal_hook(OriginalRequest, Goal)
        ; Goal
        ).
        

react_cleanup(Goal, exit, WebSocket):-
        send_reply(WebSocket, cut(Goal)).

react_cleanup(Goal, external_exception(E), WebSocket):-
	react_cleanup(Goal, WebSocket, exception(E)).

react_cleanup(_Goal, exception(E), WebSocket):-
	( E = error(Error, Context)->
	    format(atom(ContextAtom), '~p', [Context]),
	    send_reply(WebSocket, exception(error(Error, ContextAtom)))
	; E = application_error(Error, Cause, Context)->
	    format(atom(ContextAtom), '~p', [Context]),
	    format(atom(CauseAtom), '~p', [Cause]),
	    send_reply(WebSocket, exception(application_error(Error, CauseAtom, ContextAtom)))
	; otherwise->
	    send_reply(WebSocket, exception(E))
	).


react_cleanup(_Goal, fail, WebSocket):-
        send_reply(WebSocket, fail).

react_cleanup(Goal, !, WebSocket):-
        send_reply(WebSocket, cut(Goal)).

send_reply(WebSocket, Term):-
        format(atom(Text), '~k', [Term]),
	ws_send(WebSocket, text(Text)).

trigger_react_recompile(Module):-
        forall(react_listener(Queue),
               thread_send_message(Queue, text(Module))).

jsx(Form, jsx(Form)):- !.
jsx(Form, jsx(Form, Goals)):- Goals.


:-meta_predicate(user:on_server(0)).
user:on_server(X):- X.

raise_event(Key, _):- permission_error(raise, server_side_event, Key).
wait_for(List):- permission_error(wait_for, server_side_event, List).

bubble_event(List, Key, Event):-
        ( memberchk(Key=Handler, List),
          Handler \== {null}->
            call(Handler, Event)
        ; otherwise->
            true
        ).

user:term_expansion(requires(X), [depends_on(X), :-use_module(X)]).

vdiff_warning(X):-
        format(user_error, 'Warning: ~w', [X]).

