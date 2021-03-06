otherwise.

jsx(Form, jsx(Form)):- !.
jsx(Form, jsx(Form, Goals)):- Goals.


bubble_event(List, Key, Event):-
	'.'(List, Key, Handler),
	( Handler \== {null}->
            bubble_event(Handler, Event)
        ; otherwise->
            true
        ).

bubble_test(List, Key, Event):-
	'.'(List, Key, Handler),
	( Handler \== {null}->
            bubble_test(Handler, Event)
        ; otherwise->
            true
        ).

between(Low, High, I):-
        High >= Low,
        ( I = Low
        ; II is Low+1,
          between(II, High, I)
        ).

vdiff_warning(X):-
        writeln(X).

debug_message(X):-
        writeln(X).

:-meta_predicate(on_server(0)).

on_server(Goal):-
        '_on_server'(Goal).

string(_):- fail.

expand_children(A, B):-
        expand_children([A], [B], []).

expand_children([], T, T):- !.
expand_children([element(A, B, C)|D], [element(A,B,CC)|DD], T):- !,
        expand_children(C, CC, []),
        expand_children(D, DD, T).
expand_children([widget(A, B, C)|D], [widget(A,B,CC)|DD], T):- !,
        expand_children(C, CC, []),
        expand_children(D, DD, T).
expand_children([list([])|D], AA, T):- !,
        expand_children(D, AA, T).
expand_children([list([A|As])|D], AA, T):- !,
        expand_children([A], AA, T1),
        expand_children(As, T1, T2),
        expand_children(D, T2, T).
expand_children([[]|D], AA, T):- !,
        expand_children(D, AA, T).
expand_children([[A|As]|D], AA, T):- !,
        expand_children([A], AA, T1),
        expand_children(As, T1, T2),
        expand_children(D, T2, T).
expand_children(Other, _, _):-
        writeq(failed_to_expand(Other)), nl, fail.


:-meta_predicate(show(0, 0, +, +)).
show(Goal, Goal, explicit, _):-
        !,
        setup_call_catcher_cleanup(format(user_error, 'CALL ~q~n', [Goal]),
                                   Goal,
                                   Catcher,
                                   ( Catcher == fail ->
                                       format(user_error, 'FAIL ~q~n', [Goal])
                                   ; Catcher == exit ->
                                       format(user_error, 'EXIT ~q~n', [Goal])
                                   ; Catcher == ! ->
                                       format(user_error, 'CUT  ~q~n', [Goal])
                                   ; Catcher = error(Error)->
                                       format(user_error, 'ERROR ~q ~p~n', [Goal, Error])
                                   )),
        ( var(Catcher)->
            format(user_error, 'PEND ~q~n', [Goal])
        ; otherwise->
            true
        ).

show(Goal, Goal, minimal, _):-
        !,
        functor(Goal, Functor, Arity),
        setup_call_catcher_cleanup(format(user_error, 'CALL ~q~n', [Functor/Arity]),
                                   Goal,
                                   Catcher,
                                   ( Catcher == fail ->
                                       format(user_error, 'FAIL ~q~n', [Goal])
                                   ; Catcher == exit ->
                                       format(user_error, 'EXIT ~q~n', [Functor/Arity])
                                   ; Catcher == ! ->
                                       format(user_error, 'CUT  ~q~n', [Functor/Arity])
                                   ; Catcher = error(Error)->
                                       format(user_error, 'ERROR ~q ~p~n', [Functor/Arity, Error])
                                   )),
        ( var(Catcher)->
            format(user_error, 'PEND ~q~n', [Functor/Arity])
        ; otherwise->
            true
        ).

show(Goal, Goal, full, _):-
        !,
        setup_call_catcher_cleanup(format(user_error, 'CALL ~w~n', [Goal]),
                                   Goal,
                                   Catcher,
                                   ( Catcher == fail ->
                                       format(user_error, 'FAIL ~w~n', [Goal])
                                   ; Catcher == exit ->
                                       format(user_error, 'EXIT ~w~n', [Goal])
                                   ; Catcher == ! ->
                                       format(user_error, 'CUT  ~w~n', [Goal])
                                   ; Catcher = error(Error)->
                                       format(user_error, 'ERROR ~w ~p~n', [Goal, Error])
                                   )),
        ( var(Catcher)->
            format(user_error, 'PEND ~w~n', [Goal])
        ; otherwise->
            true
        ).

:-op(920, fy, ?).
:-op(920, fy, ??).

:-meta_predicate(??(0)).
:-meta_predicate(?(0)).
?(Goal):- show(Goal, Goal, minimal, _).
??(Goal):- show(Goal, Goal, explicit, _).

updateMessageHandlers(Module, Current, State, Props, Changes, Handlers):-
        findall(handle(Key, Discriminant, Handler),
                Module:onMessage(State, Props, Key, Discriminant, Handler),
                Handlers),
        findall(Term,
                ( member(Added, Handlers),
                  \+member(Added, Current),
                  Term = +(Added)
                ; member(Deleted, Current),
                  \+member(Deleted, Handlers),
                  Term = -(Deleted)
                ),
                Changes),
        Changes \== [].