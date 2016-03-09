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
