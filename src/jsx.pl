:-module(jsx, [jsx/4]).

:- quasi_quotation_syntax(jsx).

jsx(Content, [Variable], Dict, Term):-
        phrase_from_quasi_quotation(jsx_children(Dict, [DOM], Goals, true, NotSingletons, true), Content),
        ( Goals == true->
            Term = (NotSingletons, Variable = DOM)
        ; otherwise->
            Term = (NotSingletons, Goals, Variable = DOM)
        ).

garbage(X,_):-
        append(X, [], Codes),
        %length(Codes, 10)
        %append(Codes, _, X),
        ( ground(Codes)->
            atom_codes(Atom, Codes),
            throw(garbage(Atom))
        ; otherwise->
            throw(garbage)
        ).

tag_mismatch(Close, Tag, X, _):-
        append(X, [], Codes),
        atom_codes(Atom, Codes),
        throw(tag_mismatch(Close, Tag, Atom)).

:-multifile(jsx:jsx_expansion/2).

jsx_node(Dict, FinalNode, Goals, GoalsTail, Singletons, SingletonsTail) -->
        optional_spaces,
        `<`,
        jsx_tag(Tag), optional_spaces, jsx_attributes(Dict, Attributes, Goals, G1),
        { ( prolog_load_context(module, Module),
            current_predicate(Module:depends_on/1),           
            Module:depends_on(Tag)->
             Node = widget(Tag, Attributes, Content)
          ; Node = element(Tag, Attributes, Content)
          )},
        ( `/>` ->
            {Content = [],
             G1 = GoalsTail,
             SingletonsTail = Singletons}
        ; `>` ->
            jsx_children(Dict, Content, G1, GoalsTail, Singletons, SingletonsTail),
            optional_spaces,
            `</`, jsx_tag(Close), `>`,
            ( {Close == Tag}->
                {true}
            ; tag_mismatch(Close, Tag)
            )
        ; {otherwise}->
            garbage
        ),
        optional_spaces,
        expand_jsx(Node, FinalNode).

expand_jsx(Node, FinalNode, T, T):-
        ( jsx_expansion(Node, N1)->
            FinalNode = N1 %expand_jsx(N1, FinalNode)
        ; FinalNode = Node
        ).


comment --> `%`,
        !,
        single_line_comment, optional_spaces.

comment --> `/*`, % */
        !,
        multi_line_comment, optional_spaces.

single_line_comment-->
        `\n`, !.
single_line_comment-->
        [_], single_line_comment.

multi_line_comment-->
        `*/`, !.
multi_line_comment-->
        [_], multi_line_comment.


spaces --> [Code], {code_type(Code, space)}, optional_spaces.
optional_spaces --> spaces ; comment ; [].

jsx_tag(Tag)-->
        jsx_atom(Tag).

jsx_atom(Atom)-->
        jsx_atom_code(Code),
        jsx_atom_codes(Codes),
        {atom_codes(Atom, [Code|Codes])}.

jsx_atom_code(Code)-->
        [Code],
        {code_type(Code, csym) ; Code == 45}.

jsx_atom_codes([Code|Codes])-->
        jsx_atom_code(Code),
        !,
        jsx_atom_codes(Codes).
jsx_atom_codes([])--> [].

jsx_children(Dict, Children, Goal, GoalTail, Singletons, SingletonsTail)-->
        % This allows terms like {A|Tail}
        optional_spaces,
        `{`,
          optional_spaces,
          variable_name(HeadName),
          optional_spaces,
          `|`,
          !,
          optional_spaces,
          variable_name(TailName),
          optional_spaces,
        `}`,
        !,
        {memberchk(HeadName=List, Dict),
         memberchk(TailName=TailVar, Dict),
         Children = List,
         Goal = G1,
         TailVar = Tail,
         Singletons = ((TailVar = TailVar), S1)},
        jsx_children(Dict, Tail, G1, GoalTail, S1, SingletonsTail).

jsx_children(Dict, Children, Goal, GoalTail, Singletons, SingletonsTail)-->
        % This allows terms like {A}, {[A, B]} and {Props.children}
        optional_spaces,
        `{`,
        read_until_close_brace(Codes, 1),
        `}`,
        !,
        {read_term_from_atom(Codes, Term, [variable_names(TermVariableNames)]),
         unify_variables(TermVariableNames, Dict),
         ( var(Term)->
             Goal = ((var(Term)->
                        throw(error(instantiation_error(Term), _))
                     ; Term == [] ->
                        Children = Tail
                     ; is_list(Term)->
                        once(append(Term, Tail, Children))
                     ; Children = [Term|Tail]
                     ), G1)
         ; expand_goals(Term, List, G2, G1),
           Goal = (once(append(List, Tail, Children)), G2)
         )},
        jsx_children(Dict, Tail, G1, GoalTail, Singletons, SingletonsTail).

jsx_children(Dict, Children, Goal, GoalTail, Singletons, SingletonsTail)-->
        % call(+DCG)
        % Called as call(DCG, Children, Tail)
        optional_spaces,
        `call(`, !,
         read_until_close_paren(Codes, [], 1),
        `)`,
        {read_term_from_atom(Codes, Term, [variable_names(TermVariableNames)]),
         unify_variables(TermVariableNames, Dict),
         Goal = (call(Term, Children, T1), G1)},
        jsx_children(Dict, T1, G1, GoalTail, Singletons, SingletonsTail).

jsx_children(Dict, Children, Goal, GoalTail, Singletons, SingletonsTail)-->
        % findall(+Template, +Goal)
        % Called as findall(Template, Goal, Children, Tail)
        optional_spaces,
        `findall(`, !,
         read_until_close_paren(Codes, [41], 1),
        `)`,
        {read_term_from_atom([102, 105, 110, 100, 97, 108, 108, 40|Codes], Term, [variable_names(TermVariableNames)]),
         Term = findall(Template, BagGoal),
         unify_variables(TermVariableNames, Dict),
         Goal = (findall(Template, BagGoal, Children, T1), G1)},
        jsx_children(Dict, T1, G1, GoalTail, Singletons, SingletonsTail).


jsx_children(Dict, [Element|Elements], Goal, GoalTail, Singletons, SingletonsTail)-->
        jsx_node(Dict, Element, Goal, G1, Singletons, S1),
        !,
        jsx_children(Dict, Elements, G1, GoalTail, S1, SingletonsTail).

jsx_children(_Dict, [], G, G, S, S)--> [].


jsx_attributes(Dict, [Name=Value|Attributes], Goals, GoalsTail)-->
        jsx_atom(Name), `=`, jsx_value(Value, Dict, Goals, G1),
        ( spaces,
          jsx_attributes(Dict, Attributes, G1, GoalsTail)->
            !
        ; {Attributes = [],
           G1 = GoalsTail}
        ).
jsx_attributes(_, [], G, G)--> [].

read_until_close_paren(T, T, 1, [41|C], [41|C]):- !.
read_until_close_paren([40|Codes], T, N)-->
        `(`, !,
        {NN is N+1},
        read_until_close_paren(Codes, T, NN).

read_until_close_paren([41|Codes], T, N)-->
        `)`, !,
        {NN is N-1},
        read_until_close_paren(Codes, T, NN).

read_until_close_paren([Code|Codes], T, N)-->
        [Code],
        read_until_close_paren(Codes, T, N).

read_until_close_brace([], 1, [125|C], [125|C]):- !.
read_until_close_brace([123|Codes], N)-->
        `{`, !,
        {NN is N+1},
        read_until_close_brace(Codes, NN).

read_until_close_brace([125|Codes], N)-->
        `}`, !,
        {NN is N-1},
        read_until_close_brace(Codes, NN).

read_until_close_brace([Code|Codes], N)-->
        [Code],
        read_until_close_brace(Codes, N).


unify_variables([], _):- !.
unify_variables([Name=Value|Vars], Dict):-
        ignore(memberchk(Name=Value, Dict)),
        unify_variables(Vars, Dict).

jsx_value(Value, Dict, Goals, GoalsTail)-->
        `{`, !,
          read_until_close_brace(Codes, 1),
          {read_term_from_atom(Codes, Term, [variable_names(TermVariableNames)]),
           unify_variables(TermVariableNames, Dict),
           expand_goals(Term, Value, Goals, GoalsTail)},
         `}`.

jsx_value(Value, _Dict, G, G)-->
        quoted_string(Value).

expand_goals(Variable, Variable, Tail, Tail):- var(Variable), !.
expand_goals(Map, '$this'(This, Value), (get_this(This), Tail), Tail):-
        functor(Map, '.', 2),
        Map =.. ['.', Key, Value],
        Key == this, !.
expand_goals(Map, Value, (T1, '.'(Source, Key, Value)), Tail):-
        functor(Map, '.', 2), !,
        % Object could also be a ./3 term
        Map =.. ['.', Object, Key],
        expand_goals(Object, Source, T1, Tail).

expand_goals([], [], Tail, Tail):- !.
expand_goals([Head|Tail], [NewHead|NewTail], Goals, GoalTail):-
        !,
        expand_goals(Head, NewHead, Goals, G1),
        expand_goals(Tail, NewTail, G1, GoalTail).
expand_goals(X, X, Tail, Tail).


variable_name(VarName)-->
        optional_spaces, 
        [Code],
        {code_type(Code, prolog_var_start)},
        variable_name_continuation(Codes),
        {atom_codes(VarName, [Code|Codes])}.

variable_name_continuation([Code|Codes])-->
        [Code],
        {code_type(Code, prolog_identifier_continue)},
        !,
        variable_name_continuation(Codes).
variable_name_continuation([])--> [].

quoted_string(Atom)-->
        [39], !, codes_until(39, Codes), [39], {atom_codes(Atom, Codes)}.
quoted_string(Atom)-->
        [34], !, codes_until(34, Codes), [34], {atom_codes(Atom, Codes)}.

codes_until(Stop, [], [Stop|X], [Stop|X]):- !.
codes_until(Stop, [Code|Codes], [Code|In], Rest):-
        codes_until(Stop, Codes, In, Rest).
         
        