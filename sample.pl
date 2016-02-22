render_Element(_, _, element('Panel',[layout=horiztonal, fill=horizontal], [element('Title',[label='This is my title from Prolog!'],[]),
                                                                         element('Field',[label='CP Code'],[]),
                                                                         element('Field',[label='CP Account ID'],[]),
                                                                         element('Button',[label='Submit'],[])])).

render_SecondElement(_, _, element('Panel',[layout=horiztonal, fill=horizontal], [element('Title',[label='This is a totally different title!'],[]),
                                                                               element('Field',[label='CP Code'],[]),
                                                                               element('Field',[label='CP Account ID'],[]),
                                                                               element('Button',[label='Still Submit'],[])])).
