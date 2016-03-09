:-module(jsx, [jsx/4]).

:- quasi_quotation_syntax(jsx).

jsx(Content, Vars, Dict, Term):-
        phrase_from_quasi_quotation(jsx_children(Vars, Dict, [DOM], Goals, true), Content),
        ( Goals == true->
            Term = jsx(DOM)
        ; otherwise->
            Term = jsx(DOM, Goals)
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

qq(X, X):-
        writeln(X).

jsx_node(Vars, Dict, element(Tag, Attributes, Content), Goals, GoalsTail) -->
        optional_spaces,
        `<`,
        jsx_tag(Tag), optional_spaces, jsx_attributes(Vars, Dict, Attributes, Goals, G1),
        % {compile_aux_clauses([jsx_require(Tag)])}, This causes swipl to crash :(
        ( `/>` ->
            {Content = [],
             G1 = GoalsTail}
        ; `>` ->
            jsx_children(Vars, Dict, Content, G1, GoalsTail),
            optional_spaces,
            `</`, jsx_tag(Close), `>`,
            ( {Close == Tag}->
                {true}
            ; tag_mismatch(Close, Tag)
            )
        ; {otherwise}->
            garbage
        ),
        optional_spaces.

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

jsx_children(Vars, Dict, Children, Goal, GoalTail)-->
        % {Foo}. Note that {[Foo, Bar]} is not supported (yet)
        optional_spaces,
        `{`,
          !,
          % Array of components. We have to have a pointer to the siblings, which is why we do it in jsx_children
          % rather than jsx_node
          variable_name(HeadName),
          optional_spaces,
          {memberchk(HeadName=List, Dict)},
          optional_spaces,
          {Goal = ((List == [] ->
                     Children = Tail
                  ; is_list(List)->
                     append(List, Tail, Children)
                  ; Children = [List|Tail]
                  ), G1)},
        `}`,
        jsx_children(Vars, Dict, Tail, G1, GoalTail).

jsx_children(Vars, Dict, [Element|Elements], Goal, GoalTail)-->
        jsx_node(Vars, Dict, Element, Goal, G1),
        !,
        jsx_children(Vars, Dict, Elements, G1, GoalTail).

jsx_children(_Vars, _Dict, [], G, G)--> [].


jsx_attributes(Vars, Dict, [Name=Value|Attributes], Goals, GoalsTail)-->
        jsx_atom(Name), `=`, jsx_value(Value, Vars, Dict, Goals, G1),
        ( spaces,
          jsx_attributes(Vars, Dict, Attributes, G1, GoalsTail)->
            !
        ; {Attributes = [],
           G1 = GoalsTail}
        ).
jsx_attributes(_, _, [], G, G)--> [].

jsx_value(Value, Vars, Dict, Goals, GoalsTail)-->
        `{`, !,
          jsx_term(Value, Vars, Dict, Goals, GoalsTail),
         `}`.

jsx_value(Value, _Vars, _Dict, G, G)-->
        quoted_string(Value).

% Variable
jsx_term(Value, _Vars, Dict, Goals, GoalsTail)-->
        optional_spaces,
        variable_name(VarName),
        {memberchk(VarName=Variable, Dict)},
        !,
        ( `.` ->
            % Fake maps
            jsx_atom(Key),
            {Goals = ((memberchk(Key=Value, Variable)->true ; otherwise->Value = {null}), GoalsTail)}
        ; {Variable = Value, Goals = GoalsTail}
        ).

% this pointer
jsx_term(react_handler(This, Value), Vars, Dict, Goals, GoalsTail)-->
        optional_spaces,
        `this.`,
        {Goals = (get_this(This), G1)},
        !,
        jsx_term(Value, Vars, Dict, G1, GoalsTail).

% Atom and compound
jsx_term(Value, Vars, Dict, Goals, GoalsTail)-->
        optional_spaces,
        jsx_atom(Atom),
        ( `(` ->
            % compound
            optional_spaces,
            jsx_term_args(Args, Vars, Dict, Goals, GoalsTail),
            `)`,
            {Value =.. [Atom|Args]}
        ; {Value = Atom,
           GoalsTail = Goals}
        ).

jsx_term_args([Value|Args], Vars, Dict, Goals, GoalsTail)-->
        optional_spaces,
        jsx_term(Value, Vars, Dict, Goals, G1),
        !,
        optional_spaces,
        ( `,` ->
            jsx_term_args(Args, Vars, Dict, G1, GoalsTail)
        ; {Args = [], G1 = GoalsTail}
        ).
jsx_term_args([], _, _, G, G)--> [].
        
        

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
         
        