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
        <Panel fill="both" layout="vertical">
          <FakeTestWidget onSplunge={this.splunge}/>
          <Label label={State.message}/>
          <Title label="Tech Demo" fill="none" align-children="center"/>
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
                    message: initial}).

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





%% Tests


xdo_test:-
        proactive(test_demo, proactive{}, Document),
        find_a_thing(Document, Root),
        show_splunge(Root),
        writeln(triggering_event_now),
        proactive_event(Root, onSplunge, [qux=bing]),
        find_a_thing(Document, Root2),
        show_splunge(Root2),
        render_document(user_error, Document).

show_splunge(dom_element(A)):-
        memberchk(properties-P, A),
        get_attr(P, react_dom, Props),
        memberchk(onSplunge='$this'(XXX, _), Props),
        get_attr(XXX, react_dom, This),
        writeln(splunge=This).


do_test:-
        proactive(test_demo, proactive{}, Document),
        find_a_thing(Document, Root),
        find_a_thing(Root, Thing),
        proactive_event(Thing, onSplunge, [new_message=after_event_1]),
        proactive_event(Thing, onSplunge, [new_message=after_event_2]),
        show_splunge(Thing),
        render_document(user_error, Document).

% This will be replaced by vPath eventually
find_a_thing(DOM, Thing):-
        DOM = dom_element(A),
        memberchk(children-Ptr, A),
        get_attr(Ptr, react_dom, Children),
        memberchk(Thing, Children).
