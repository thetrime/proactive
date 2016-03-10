package org.proactive.vdom;

import gnu.prolog.term.Term;
import org.proactive.ReactComponent;
import org.proactive.ReactComponentFactory;
import org.proactive.prolog.FluxDispatcher;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologState;

public class PrologWidget extends PrologElement
{
   private String componentName;
   private PrologContext widgetContext;
   
   public PrologWidget(String componentName) throws Exception
   {
      // constructs a widget out of JUST a componentName.
      // Necessary to bootstrap the entire tree
      super();
      this.componentName = componentName;
   }
   
   public PrologWidget(Term t, String componentName) throws Exception
   {
      super(t);
      this.componentName = componentName;
   }
   public ReactComponent init(PrologContext context) throws Exception
   {
      return null;
   }
   public ReactComponent update(PrologNode vNode, ReactComponent oldComponent)
   {
      // FIXME: stub
      System.out.println("Warning: update() called on widget of type " + componentName);
      return oldComponent;
   }
   public void destroy(ReactComponent domNode)
   {
      System.out.println("Destroyed a widget of type " + componentName + " from " + domNode);
      FluxDispatcher.deregisterFluxListener(componentName, widgetContext);
   }
   
   @Override   
   public boolean hasWidgets()
   {
      return true;
   }   

   public boolean equals(Object o)
   {
      if (o instanceof PrologWidget)
      {
         PrologWidget other = (PrologWidget)o;
         return componentName.equals(other.componentName) &&
            attributes.equals(other.attributes);
      }
      return false;
   }

}
