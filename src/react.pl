:-module(react,
         [raise_event/2,
          wait_for/1,
          broadcast_proactive_message/1,
          trigger_react_recompile/1,
          vdiff_warning/1,
          related_react_module/2,
          react_system_message/2,
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
:-http_handler(root('react/listen'), listen_react, [spawn([])]).
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
                    related_react_module(Module, Candidate),
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
                % We shouldnt close the CGI-stream ourself. The wrapper will do that when the handler is finished
                zopen(current_output, TargetStream, [close_parent(false)])
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
        functor(Head, Name, Arity),
        \+predicate_property(Module:Head, imported_from(_)),
        % Do not provide the source code of tabled predicates if they are also in the same module
        ( current_predicate(_, Module:tabled_predicate(_, _))->
            \+clause(Module:tabled_predicate(Module, Name/Arity), true)
        ; true
        ),
        clause(Module:Head, Body, _).
react_clause(Module, Head):-
        current_predicate(_, Module:tabled_predicate(_, _)),
        clause(Module:tabled_predicate(SourceModule, Name/Arity), true),
        functor(Head, Name, Arity),
        call(SourceModule:Head).


%!      related_react_module(+BaseModule, ?OtherModule) is semidet.
%       Succeeds if OtherModule is a module which BaseModule depends on
%       If OtherModule is unbound, on backtracking all modules related to the root
%       are enumerated.
%       Note that BaseModule is defined to be dependent on BaseModule itself. This tends
%       to make code which finds all code related to a particular module simpler to implement

related_react_module(Root, Root).
related_react_module(Module, Related):-
        current_predicate(_, Module:depends_on(_)),
        predicate_property(Module:depends_on(_), interpreted),
        \+predicate_property(Module:depends_on(_), imported_from(_)),
        clause(Module:depends_on(SubModule), _, _),
        related_react_module(SubModule, Related).



listen_react(Request):-
        ( http_in_session(SessionID)->
            true
        ; SessionID = {null}
        ),
        http_upgrade_to_websocket(listen_react_loop(SessionID), [], Request).

listen_react_loop(SessionID, Websocket):-
        ( SessionID == {null}->
            true
        ; b_setval(http_session_id, SessionID)
        ),
        thread_self(Self),
        ws_receive(Websocket, Message),
        ( Message.opcode == text->
            Data = Message.data,
            read_term_from_atom(Data, RootComponent, []),
            thread_create(ws_listen_slave(Websocket, Self), Slave, [detached(false)]),
            setup_call_cleanup(open_react_session(Self, RootComponent),
                               listen_react_loop_1(Websocket, Slave),
                               close_react_session(Self))
        ; otherwise->
            true
        ).

:-multifile(open_react_session_hook/1).
:-multifile(close_react_session_hook/1).

open_react_session(Self, RootComponent):-
        forall(open_react_session_hook(Self), true),
        ( setof(RelatedModule,
                related_react_module(RootComponent, RelatedModule),
                Modules)->
            true
        ; Modules = []
        ),
        assert(react_listener(Self, Modules)).

close_react_session(Self):-
        retractall(react_listener(Self, _)),
        forall(close_react_session_hook(Self), true).



:-multifile(react:react_message_hook/1).

:-thread_local(react_client_handles_message/2).

listen_for_messages([]):- !.

listen_for_messages([-handle(Key, _, Handler)|Keys]):-
        retractall(react_client_handles_message(Key, Handler)),
        listen_for_messages(Keys).

listen_for_messages([+handle(Key, Discriminant, Handler)|Keys]):-
        assert(react_client_handles_message(Key, Handler) :- Discriminant),
        listen_for_messages(Keys).

ws_listen_slave(Websocket, Owner):-
        ws_receive(Websocket, Message),
        ( Message.opcode == close->
            thread_send_message(Owner, close)
        ; Message.opcode == text->
            Data = Message.data,
            read_term_from_atom(Data, Term, []),
            ( Term = message(T)->
                ( catch(react_message_hook(T),
                        Exception,
                        format(user_error, 'Exception handling Proactive message ~q: ~p~n', [T, Exception]))->
                    true
                ; format(user_error, 'Failure handling Proactive message ~q~n', [T])
                )
            ; Term = listen_for(T)->
                thread_send_message(Owner, listen_for_messages(T))
            ; format(user_error, 'Unexpected message from client: ~q~n', [Term])
            ),
            ws_listen_slave(Websocket, Owner)
        ; otherwise->
            ws_listen_slave(Websocket, Owner)
        ).

listen_react_loop_1(Websocket, Slave):-
        thread_get_message(Message),
        ( Message == close->
            thread_join(Slave, _),
            ws_close(Websocket, 1000, goodbye),
            throw(terminated)
        ; Message = message(Module, Term)->
            copy_term(Term, Copy),
            ( react_client_handles_message(Copy, Handler)->
                format(atom(Text), '~k', [message(Module, Handler, Term)]),
                ws_send(Websocket, text(Text))
            ; true
            )
        ; Message = listen_for_messages(Handlers)->
            listen_for_messages(Handlers)
        ; Message = consulted(_)->
            format(atom(Text), 'system(~k)', [Message]),
            ws_send(Websocket, text(Text))
        ; Message = system(_)->
            format(atom(Text), '~k', [Message]),
            ws_send(Websocket, text(Text))
        ; format(user_error, 'Bad proactive message: ~q~n', [Message])
        ),
        !,
        listen_react_loop_1(Websocket, Slave).

react_system_message(SessionID, Message):-
        thread_send_message(SessionID, system(Message)).

:-dynamic(react_listener/2).

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
            catch(read_react_goal_and_execute_if_safe(WebSocket, OriginalRequest, Data),
                  Exception,
                  ( handle_react_goal_exception(Exception, WebSocket),
                    throw(Exception)
                  ))
        ; Message.opcode == close->
            !
        ).

read_react_goal_and_execute_if_safe(WebSocket, OriginalRequest, Data):-
            read_term_from_atom(Data, Goal, []),
            ( goal_is_safe(Goal)->
                execute_react_ws(OriginalRequest, WebSocket, Goal, Goal)
            ; permission_error(execute, goal, Goal)
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

react_cleanup(_Goal, exception(_), _):- true. % Handled later in handle_react_goal_exception/2.

react_cleanup(_Goal, fail, WebSocket):-
        send_reply(WebSocket, fail).

react_cleanup(Goal, !, WebSocket):-
        send_reply(WebSocket, cut(Goal)).


handle_react_goal_exception(E, WebSocket):-
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


send_reply(WebSocket, Term):-
        format(atom(Text), '~k', [Term]),
	ws_send(WebSocket, text(Text)).

trigger_react_recompile(Module):-
        forall(react_listener(Queue, Modules),
               ( member(Module, Modules)->
                   thread_send_message(Queue, consulted(Module))
               ; true
               )).

jsx(Form, jsx(Form)):- !.
jsx(Form, jsx(Form, Goals)):- Goals.


:-meta_predicate(user:on_server(0)).
user:on_server(X):- X.

raise_event(Key, _):- permission_error(raise, server_side_event, Key).
wait_for(List):- permission_error(wait_for, server_side_event, List).

user:term_expansion(requires(X), [depends_on(X), :-react:do_load_react_module(X)]).

:-multifile(react:load_react_module/1).

do_load_react_module(X):-
        ( load_react_module(X)->
            true
        ; use_module(X)
        ).


vdiff_warning(X):-
        format(user_error, 'Warning: ~w', [X]).


% Messaging
broadcast_proactive_message(Term):-
        forall((react_listener(Listener, Components),
                member(Component, Components),
                % If the client has any module which can handle any message, send the message to its worker thread
                % the worker thread will take additional steps (with context that only the worker should know) to decide
                % whether it needs to be passed on or not
                current_predicate(Component:onMessage/5)),
               thread_send_message(Listener, message(Component, Term))).
