my_event_handler(_State, _Props, [something='Did this just change?']).

render_App(_, _, element('Panel',[layout=horiztonal, fill=horizontal], [element('Title',[label='This is a title from Prolog'],[]),
                                                                        element('Splunge', [some_property='This is a property'], []),
                                                                        element('Field',[label='CP Code'],[]),
                                                                        element('Field',[label='CP Account ID'],[]),
                                                                        element('Button',[label='Submit'],[])])).

render_SecondElement(_, _, element('Panel',[layout=horiztonal, fill=horizontal], [element('Title',[label='This is a totally different title!'],[]),
                                                                                  element('Field',[label='CP Code'],[]),
                                                                                  element('Field',[label='CP Account ID'],[]),
                                                                                  element('Button',[label='Still Submit'],[])])).

render_Splunge(State, Props, element('Panel', [layout=horizontal], [element('Title', [label=Label], []),
                                                                    element('Button', [label=Something, onClick=my_event_handler], [])])):-
        memberchk(some_property=Label, Props),
        memberchk(something=Something, State).


getInitialState_Splunge([something='This is determined by state!']).
