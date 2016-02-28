package org.proactive;

import java.util.List;
import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;

public abstract class ReactLeafComponent extends ReactComponent
{
   public ReactLeafComponent(PrologContext context)
   {
      super(context);
   }
   public void insertChildBefore(ReactComponent child, ReactComponent sibling) {}
   public void removeChild(ReactComponent child) {}
   public void replaceChild(ReactComponent newChild, ReactComponent oldChild) {}
   public List<ReactComponent> getChildNodes() { return null; }
}
