:-module(test_demo, []).

requires('Title').

render(State, _Props, Form) :-
        on_server(UserId = tester),
        format(atom(Message), 'Hello ~w, and welcome to the tech demo', [UserId]),
        ( State.chart_type == pie ->
            IsPie = boolean(true)
        ; IsPie = boolean(false)
        ),
        {|jsx(Form)||
        <Panel fill="both" layout="vertical" eventData={State.event_data} >
          <FakeTestWidget onSplunge={splunge}/>
          <Title label="Tech Demo" fill="none" align-children="center" onFoo={this.onFoo} onBar={this.onBar} subordinate_data={State.event_data}/>
          <Label label={State.message}/>
          <Label label={Message}/>
          <Panel layout="vertical" fill="none">
            <LabelledField label="User ID" value={UserId}/>
            <Panel layout="horizontal" align-children="end" fill="both" justify-content="space-between">
              <Label label="HTML Text" fill="none"/>
              <Label label="<html><body>This is <b>some</b> HTML text inside <u>a label with some <h1>big text</h1> at the end</u></body></html>"/>
            </Panel>
          </Panel>
          <Panel layout="horizontal" fill="both">
            <Panel layout="vertical" fill="both" scroll="both">
              <Chart width="350" height="350" type={State.chart_type} labels={['Ant', 'Bear', 'Cat', 'Dog']} onClick={this.chartClick}>
                <DataSet label="Scenario A" colour="#712A00">
                  <Data value={100} colour='#712A00'/>
                  <Data value={400} colour='#714600'/>
                  <Data value={180} colour='#02224A'/>
                  <Data value={180} colour='#004937'/>
                </DataSet>
                <DataSet label="Scenario B" colour="#714600">
                  <Data value={400} colour='#712A00'/>
                  <Data value={200} colour='#714600'/>
                  <Data value={500} colour='#02224A'/>
                  <Data value={190} colour='#004937'/>
                </DataSet>
                <DataSet label="Scenario C" colour="#02224A">
                  <Data value={600} colour='#712A00'/>
                  <Data value={250} colour='#714600'/>
                  <Data value={250} colour='#02224A'/>
                  <Data value={300} colour='#004937'/>
                </DataSet>
              </Chart>
              <Panel layout="horizontal" fill="both">
                <LabelledField label="Display as a pie chart" type="checkbox" value={IsPie} onBlur={this.toggleChartType}/>
                <Button label="Click me!" onClick={handleClick}/>
              </Panel>
            </Panel>
            <Panel layout="vertical" fill="both">
              list(Tables)
            </Panel>
          </Panel>
        </Panel>|},
        aggregate(r(bag(Table)), test_demo:table(Table), r(Tables)).


table(Table) :-
        aggregate(r(set(Row), sum(N)), test_demo:row(Row, N, Size), r(Rows, _TotalN)),
        {|jsx(Table)||
        <Panel layout="vertical" fill="both" scroll="both">
          <Label label={Size}/>
          <Table fill="both">
            <TableHeader><Button label="Name" fill="horizontal"/><Button label="Favourite Animal"/><Button label="Favourite Number"/></TableHeader>
            list(Rows)
          </Table>
        </Panel>|}.

%!      row(-Row, -Size) is nondet.
row(Row, Number, Size) :-
        table_data(Name, Animal, Size, Number),
        {|jsx(Row)||<Row onDblClick={selectValue(Name)}> <Field value={Name} align="right" title="Your tooltip here"/><Label label={Animal}/><Label label={Number}/></Row>|}.

selectValue(Name, _Event, _State, _Props, {}) :-
        writeln(row_selected(Name)).

getInitialState(_, {chart_type:pie,
                    message: initial,
                    event_data: no_data_yet}).

table_data(mike, tardigrade, big, 3).
table_data(keri, lemur, small, 7).
table_data(jen, leopard, big, 7).
table_data(bill, tarsier, small, -2).
table_data(matt, marmoset, big, 19.4).
table_data(anton, ocelot, small, 30).
table_data(chris, leek, big, 30).
table_data(chris, leek, medium, 30).


chartClick(Event, _State, _Props, {}):-
        on_server(format(user_error, 'Got ~w~n', [Event])).


toggleChartType(_Event, State, _Props, {chart_type:NewType}):-
        ( State.chart_type == pie ->
            NewType = bar
        ; NewType = pie
        ).

splunge(Event, _State, _Props, {message: NewMessage}):-
        writeln(got_event(Event)),
        memberchk(new_message=NewMessage, Event).

onFoo(Event, State, _Props, {event_data: Data}):-
        writeln(test_demo(onFoo, Event, State)),
        memberchk(data=Data, Event).

onBar(Event, State, _Props, {event_data: Data}):-
        writeln(test_demo(onBar, Event, State)),
        memberchk(data=Data, Event).





%% Tests


do_test:-
        proactive(test_demo, proactive{}, Document),
        vpath(Document, 'Label'(@className=title), Title),
        !,
        proactive_event(Title, onFoo, [data=1]),
        render_document(user_error, Document),
        vpath(Document, 'Label'(@parentData=ParentData, @className=title), _),
        vpath(Document, /_/'Panel'(@eventData=EventData), _),
        vpath(Document, /_/X/'Label'(@eventData=ChildEventData), _),
        !,
        writeln(EventData->ParentData->ChildEventData),
        writeln(X).

