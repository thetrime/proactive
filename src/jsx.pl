:-module(jsx, [jsx/4]).

:- quasi_quotation_syntax(jsx).

jsx(Content, Vars, Dict, DOM):-
        phrase_from_quasi_quotation(jsx_children(Vars, Dict, [DOM]), Content).

garbage(X,_):-
        length(Codes, 10),
        append(Codes, _, X),
        ( ground(Codes)->
            atom_codes(Atom, Codes),
            throw(garbage(Atom))
        ; otherwise->
            throw(garbage)
        ).

jsx_node(Vars, Dict, element(Tag, Attributes, Content)) -->
        optional_spaces,
        `<`,
        jsx_tag(Tag), optional_spaces, jsx_attributes(Vars, Dict, Attributes),
        % {compile_aux_clauses([jsx_require(Tag)])}, This causes swipl to crash :(
        ( `/>` ->
            {Content = []}
        ; `>` ->
            jsx_children(Vars, Dict, Content),
            optional_spaces,
            `</`, jsx_tag(Tag), `>`
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

jsx_children(Vars, Dict, [list(List)|Tail])-->
        optional_spaces,
        `{`,
          !,
          % Array of components. We have to have a pointer to the siblings, which is why we do it in jsx_children
          % rather than jsx_node
          variable_name(HeadName),
          optional_spaces,
          {memberchk(HeadName=List, Dict)},
          optional_spaces,
        `}`,
        jsx_children(Vars, Dict, Tail).

jsx_children(Vars, Dict, [Element|Elements])-->
        jsx_node(Vars, Dict, Element),
        !,
        jsx_children(Vars, Dict, Elements).

jsx_children(_Vars, _Dict, [])--> [].


jsx_attributes(Vars, Dict, [Name=Value|Attributes])-->
        jsx_atom(Name), `=`, jsx_value(Value, Vars, Dict),
        ( spaces,
          jsx_attributes(Vars, Dict, Attributes)->
            !
        ; {Attributes = []}
        ).
jsx_attributes(_, _, [])--> [].

jsx_value(Value, Vars, Dict)-->
        `{`, !,
          jsx_term(Value, Vars, Dict),
         `}`.

jsx_value(Value, _Vars, _Dict)-->
        quoted_string(Value).

% Variable
jsx_term(Value, _Vars, Dict)-->
        optional_spaces,
        variable_name(VarName),
        {memberchk(VarName=Variable, Dict)},
        !,
        ( `.` ->
            % Fake maps
            jsx_atom(Key),
            {Value = '$state'(Key, Variable)}
        ; {Variable = Value}
        ).

% this pointer
jsx_term('$this'(Value), Vars, Dict)-->
        optional_spaces,
        `this.`,
        !,
        jsx_term(Value, Vars, Dict).

% Atom and compound
jsx_term(Value, Vars, Dict)-->
        optional_spaces,
        jsx_atom(Atom),
        ( `(` ->
            % compound
            optional_spaces,
            jsx_term_args(Args, Vars, Dict),
            `)`,
            {Value =.. [Atom|Args]}
        ; {Value = Atom}
        ).

jsx_term_args([Value|Args], Vars, Dict)-->
        optional_spaces,
        jsx_term(Value, Vars, Dict),
        !,
        optional_spaces,
        ( `,` ->
            jsx_term_args(Args, Vars, Dict)
        ; {Args = []}
        ).
jsx_term_args([], _, _)--> [].
        
        

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
         
        