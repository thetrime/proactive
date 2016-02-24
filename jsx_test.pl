:-module(jsx_test, []).

:- quasi_quotation_syntax(jsx).


render_App(State, _Props, Form):-
        get_some_fields(Fields),
        Form = {|jsx||
               <Panel>
               <Title label={Label}/>
               {Fields}
               <Button label={State.label}/>
               </Panel>|},
        Label = 'This is my title'.


get_some_fields(Fields):-
        findall({|jsx||
                <Button label={Label}/>|},
                member(Label, [foo, bar, baz]),
                Fields).