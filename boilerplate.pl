memberchk(A, B):-
        once(member(A, B)).

writeln(X):-
        java_println(X).