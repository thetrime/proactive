:-module('Title', []).

render(State,Props,Form):-
        {|jsx(Form)||
        <Label label={Props.label} className="title" onFoo={Props.onBar} eventData={State.event_data} parentData={Props.subordinate_data}/>|}.

onFoo(Event, State, _Props, {event_data: Data}):-
        writeln(test_demo(onFoo, Event, State)),
        memberchk(data=Data, Event).

onBar(Event, State, _Props, {event_data: Data}):-
        writeln(test_demo(onBar, Event, State)),
        memberchk(data=Data, Event).

getInitialState(_, {info: title_state,
                    event_data: no_data_yet}).