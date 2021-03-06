:-module('Splunge', []).

:- quasi_quotation_syntax(jsx).

render(State, _Props, Form):-
	Order = State.order,
        ( Order == default->
            Label1 = top,
            Label2 = bottom,
            Key1 = top,
            Key2 = bottom
        ; otherwise->
            Key1 = bottom,
            Key2 = top,
            Label1 = bottom,
            Label2 = top
        ),
        {|jsx(Form)||
        <Panel layout="horizontal" fill="horizontal">
        <Label label={State.order}/>
        <Button key={Key1} label={Label1} onClick={swap_labels}/>
        <Button key={Key2} label={Label2} onClick={swap_labels}/>
        </Panel>|}.

getInitialState(_, {order: default}).

swap_labels(_, State, _, {order: swapped}):-
	State.order == default,
        some_local_goal,
        on_server(member(A, [a,b,c])), writeln(A), A == c.
swap_labels(_, State, _, {order: default}):-
	State.order == swapped.


some_local_goal:-
        writeln(about_to_raise),
        raise_event(set_buttons, [cat, dog, gnu, ant]),
        writeln(raised),
        some_exported_goal,
        fail.

some_local_goal:-
        writeln(other_clause).