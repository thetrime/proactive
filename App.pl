:-module('App', [some_exported_goal/0]).

:- quasi_quotation_syntax(jsx).

require('Splunge').

render(State, _Props, Form):-
        get_some_fields(Fields),
        Form = {|jsx||
               <Panel>
               <Title label={Label}/>
               {Fields}
               <Splunge foo="bar"/>
               <Button label={State.label}/>
               </Panel>|},
        Label = 'This is my title'.


get_some_fields(Fields):-
        findall({|jsx||
                <Button label={Label}/>|},
                member(Label, [foo, bar, qux, baz]),
                Fields).

getInitialState([label=boing]).


some_exported_goal:-
        writeln('Yes, this is dog'),
        some_local_goal(X),
        writeln(X).

some_local_goal(cat).
some_local_goal(dog).
some_local_goal(mouse).