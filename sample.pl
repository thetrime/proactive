/*

my_event_handler(_State, _Props, [button_label='Did this just change?']).

render_App(_, _, element('Panel',[layout=horiztonal, fill=horizontal], [element('Title',[label='This is a title from Prolog'],[]),
                                                                        element('SomeSubcomponent', [title_label='This is a property'], []),
                                                                        element('Field',[label='CP Code'],[]),
                                                                        element('Field',[label='CP Account ID'],[]),
                                                                        element('Button',[label='Submit'],[])])).

render_SomeSubcomponent(State, Props, element('Panel', [layout=horizontal, fill=horizontal], [element('Title', [label=Label], []),
                                                                                              element('Button', [label=ButtonLabel, onClick=my_event_handler], []),
                                                                                              element('Button', [label=StaticLabel], [])])):-
        memberchk(title_label=Label, Props),
        memberchk(button_label=ButtonLabel, State),
        memberchk(static_label=StaticLabel, State).


getInitialState_SomeSubcomponent([button_label='This is determined by state!',
                                  static_label='This label should not change']).

*/

render_App(State,_G153444,element('Panel',[],[element('Title',[label='This is my title'],[]),list([element('Button',[label=foo],[]),element('Button',[label=bar],[]),element('Button',[label=baz],[])]),element('Button',[label='$state'(label,State)],[])])).

getInitialState_App([label=boing]).