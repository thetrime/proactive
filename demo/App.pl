:-module('App', [some_exported_goal/0]).

:- quasi_quotation_syntax(jsx).

requires('Splunge').
requires('SomeStore').

listen_for('SomeStore', flux_handler).

flux_handler(_StoreName, StoreState, _State, _Props, StoreState):-
        writeln(ohai(StoreState)).

q(X):-
        {|jsx(X)||
        <Panel>
          {A|B}
          <Button/>
        </Panel>|},
        A = B.

raiseAnEvent(_Event, _State, _Props, []):-
        raise_event(bing, bong).

render(State, _Props, Form):-
        memberchk(buttons=Buttons, State),
        get_some_fields(Buttons, Fields),
        get_state(State, event_triggered, EventTriggered),
        writeln(event=EventTriggered),
        {|jsx(Form)||
        <Panel>
          <Label label={Label}/>
          <Button label="Click me for an event" onClick={raiseAnEvent}/>
          <Label label={EventTriggered}/>
          {Fields}
          <List fill="horizontal" onSelect={this.listSelect}>
            findall(ListItem, list_item(State, ListItem))
          </List>
          <Table fill="both">
            <TableHeader>
              <Button label="column 1"/>
              <Button label="column 2" onClick={this.columnClick(2)}/>
              <Button label="column 3"/>
              <Button label="column 4"/>
              <Button label="column 5"/>
            </TableHeader>
            {Rows|Tail}
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
                Rows,
                Tail).

list_item(State, ListItem):-
        member(Label, [foo, bar, baz, qux]),
        ( memberchk(Label=selected, State)->
            Selected = true
        ; otherwise->
            Selected = false
        ),
        {|jsx(ListItem)||
        <ListItem label={Label} key={Label} selected={Selected}/>|}.

listSelect(Event, _State, _Props, NewState):-
        memberchk(key=Key, Event),
        memberchk(isSelected=IsSelected, Event),
        ( IsSelected == true ->
            NewState = [Key=selected]
        ; otherwise->
            NewState = [Key=not_selected]
        ).

get_some_fields(buttons(Buttons), Fields):-
        findall(Field,
                ( member(Label, Buttons),
                  {|jsx(Field)||
                  <Button label={Label}/>|}
                ),
                Fields).

columnClick(Key, Event, _, _, []):-
        writeln(click(Key, Event)).

getInitialState(_, [buttons=buttons([foo, bar, qux, baz]), label='Label of button defined in state', event_triggered=false]).


some_exported_goal:-
        writeln('Yes, this is dog'),
        some_local_goal(X),
        writeln(X).

some_local_goal(cat).
some_local_goal(dog).
some_local_goal(mouse).

