:-module('App', [some_exported_goal/0]).

:- quasi_quotation_syntax(jsx).

requires('Splunge').

render(State, _Props, Form):-
        memberchk(buttons=Buttons, State),
        get_some_fields(Buttons, Fields),
        Form = {|jsx||
               <Panel>
                 <Title label={Label}/>
               {Fields}
               <Splunge foo="bar"/>
               <Button label={State.label}/>
               </Panel>|},
        Label = 'This is my title'.


get_some_fields(Buttons, Fields):-
        findall({|jsx||
                <Button label={Label}/>|},
                member(Label, Buttons),
                Fields).

getInitialState(_, [buttons=[foo, bar, qux, baz], label='Label defined in state']).


some_exported_goal:-
        writeln('Yes, this is dog'),
        some_local_goal(X),
        writeln(X).

some_local_goal(cat).
some_local_goal(dog).
some_local_goal(mouse).

handle_event(set_buttons, Term, _State, _Props, [buttons=Term]).