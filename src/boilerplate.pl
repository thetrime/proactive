
otherwise.

jsx(Form, jsx(Form)):- !.
jsx(Form, jsx(Form, Goals)):- Goals.

:-meta_predicate(call(0,?)).

call(A,B):-
	( A = Module:InGoal->
            InGoal =.. [Name|Args],
            append(Args, [B], NewArgs),
            Goal =.. [Name|NewArgs],
            call(Module:Goal)
        ; A =.. [Name|Args],
          append(Args, [B], NewArgs),
          Goal =.. [Name|NewArgs],
          call(Goal)
        ).

:-meta_predicate(call(0,?,?)).

call(A,B,C):-
	( A = Module:InGoal->
            InGoal =.. [Name|Args],
            append(Args, [B,C], NewArgs),
            Goal =.. [Name|NewArgs],
            call(Module:Goal)
        ; A =.. [Name|Args],
          append(Args, [B,C], NewArgs),
          Goal =.. [Name|NewArgs],
          call(Goal)
        ).


bubble_event(List, Key, Event):-
	'.'(List, Key, Handler),
	( Handler \== {null}->
            bubble_event(Handler, Event)
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