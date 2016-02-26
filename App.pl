:-module('App', [some_exported_goal/0]).

:- quasi_quotation_syntax(jsx).

require('Splunge').

render_App(State, _Props, Form):-
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

getInitialState_App([label=boing]).


some_exported_goal:-
        writeln('Yes, this is dog').