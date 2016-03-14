memberchk(A, B):-
        once(member(A, B)).

writeln(X):-
        java_println(X).

otherwise.

jsx(Form, jsx(Form)):- !.
jsx(Form, jsx(Form, Goals)):- Goals.

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

bubble_event(List, Key, Event):-
        ( memberchk(Key=Handler, List),
          Handler \== {null}->
            call(Handler, Event)
        ; otherwise->
            true
        ).

between(Low, High, I):-
        High >= Low,
        ( I = Low
        ; II is Low+1,
          between(II, High, I)
        ).

get_state({null}, _, {null}):- !.
get_state(Object, Key, Value):-
        ( atom(Key)->
            ( memberchk(Key=Value, Object)->
                true
            ; otherwise->
                Value = {null}
            )
        ; otherwise->
            Key =.. [Atom|Args],
            ( memberchk(Atom=ValueWithLessArgs, Object)->
                glue_args(ValueWithLessArgs, Args, Value)
            ; otherwise->
                Value = {null}
            )
        ).

glue_args(react_handler(Context, Goal), Args, react_handler(Context, NewGoal)):- !,
        ( Goal = Module:ActualGoal->
            ActualGoal =.. [Name|ExistingArgs],
            append(ExistingArgs, Args, NewArgs),
            NewActualGoal =.. [Name|NewArgs],
            NewGoal = Module:NewActualGoal
        ; otherwise->
            Goal =.. [Name|ExistingArgs],
            append(ExistingArgs, Args, NewArgs),
            NewGoal =.. [Name|NewArgs]
        ).

glue_args(Goal, Args, NewGoal):- !,
        ( Goal = Module:ActualGoal->
            ActualGoal =.. [Name|ExistingArgs],
            append(ExistingArgs, Args, NewArgs),
            NewActualGoal =.. [Name|NewArgs],
            NewGoal = Module:NewActualGoal
        ; otherwise->
            Goal =.. [Name|ExistingArgs],
            append(ExistingArgs, Args, NewArgs),
            NewGoal =.. [Name|NewArgs]
        ).