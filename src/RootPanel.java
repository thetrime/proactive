package org.proactive;

import org.proactive.vdom.PrologNode;
import org.proactive.prolog.Engine;
import org.proactive.prolog.PrologContext;

public class RootPanel extends Panel
{
    public RootPanel(String componentId, Engine engine)
    {
        super();
        context = new PrologContext(componentId, engine);
        context.setRoot(this);        
    }

}
