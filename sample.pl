my_event_handler:-
        writeln(ohai).

render_Element(_, _, element('Panel',[layout=horiztonal, fill=horizontal], [element('Title',[label='This is a title from Prolog'],[]),
                                                                            element('Splunge', [some_property='This is a property'], []),
                                                                            element('Field',[label='CP Code'],[]),
                                                                            element('Field',[label='CP Account ID'],[]),
                                                                            element('Button',[label='Submit', onClick=my_event_handler],[])])).

render_SecondElement(_, _, element('Panel',[layout=horiztonal, fill=horizontal], [element('Title',[label='This is a totally different title!'],[]),
                                                                                  element('Field',[label='CP Code'],[]),
                                                                                  element('Field',[label='CP Account ID'],[]),
                                                                                  element('Button',[label='Still Submit'],[])])).

render_Splunge(_, Props, element('Panel', [layout=horizontal], [element('Title', [label=Label], []),
                                                                element('Button', [label='This is a button inside a Splunge'], [])])):-
        memberchk(some_property=Label, Props).

memberchk(A, B):-
        once(member(A, B)).