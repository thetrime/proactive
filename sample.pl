render_Something(_, '<?xml version="1.0" encoding="ISO-8859-1"?><Panel layout="horizontal" fill="horizontal"><Title label="This is a title specified in XML" fill="horizontal"/><Field label="CP Code"/><Field label="CP Account ID"/><Button label="Submit"/></Panel>').

render_Element(_, element('Panel',[layout=horiztonal, fill=horizontal],
                          [element('Title',[label='This is my title from Prolog!'],[]),
                           element('Field',[label='CP Code'],[]),
                           element('Field',[label='CP Account ID'],[]),
                           element('Button',[label='Submit'],[])])).
