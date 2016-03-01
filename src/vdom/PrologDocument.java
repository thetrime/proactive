package org.proactive.vdom;

import gnu.prolog.term.*;
import java.util.List;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.Engine;
import org.proactive.ReactComponent;

public class PrologDocument extends PrologElement
{
   Term root;
   PrologContext context;
   String componentName;
   public PrologDocument(Term term, Term state, Term props, String componentName, Engine engine) throws Exception
   {
      super(term);
      this.componentName = componentName;
      this.context = new PrologContext(state, props, componentName, this, engine);
      this.lifecycleManager = new WidgetLifecycleManager()
         {
            public void init()
            {
               // This is never called.
               // Really we should be calling render() here, rather than recursively in PrologElement?
               System.out.println("Created a widget of type " + componentName);
            }
            public void update()
            {
            }
            public void destroy(ReactComponent domNode)
            {
               System.out.println("Destroyed a widget of type " + componentName + " from " + domNode);
            }
         };
   }
   public PrologContext getContext()
   {
      return context;
   }

   public WidgetLifecycleManager lifecycleManager = null;
   public abstract class WidgetLifecycleManager
   {
      public abstract void init();
      public abstract void update();
      public abstract void destroy(ReactComponent domNode);
        
   }
}
