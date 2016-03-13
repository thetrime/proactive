:-module('App', [some_exported_goal/0]).

:- quasi_quotation_syntax(jsx).

requires('Splunge').

render(State, _Props, Form):-
        memberchk(buttons=Buttons, State),
        get_some_fields(Buttons, Fields),
        {|jsx(Form)||
        <Panel>
        <Label label={Label}/>
        {Fields}
        <Table fill="both">
        {Rows}
        </Table>
        <Splunge foo="bar"/>
        <Button label={State.label}/>
        </Panel>|},
        Label = 'This is my title',
        findall(Row,
                ( between(0, 500, I),
                  L1 is I * 5 + 0,
                  L2 is I * 5 + 1,
                  L3 is I * 5 + 2,
                  L4 is I * 5 + 3,
                  L5 is I * 5 + 4,
                  {|jsx(Row)||
                  <Row><Label label={L1}/><Label label={L2}/><Label label={L3}/><Label label={L4}/><Label label={L5}/></Row>|}
                ),                  
                Rows).



get_some_fields(Buttons, Fields):-
        findall(Field,
                ( member(Label, Buttons),
                  {|jsx(Field)||
                  <Button label={Label}/>|}
                ),
                Fields).

getInitialState(_, [buttons=[foo, bar, qux, baz], label='Label of button defined in state']).


some_exported_goal:-
        writeln('Yes, this is dog'),
        some_local_goal(X),
        writeln(X).

some_local_goal(cat).
some_local_goal(dog).
some_local_goal(mouse).

handle_event(set_buttons, Term, _State, _Props, [buttons=Term]).