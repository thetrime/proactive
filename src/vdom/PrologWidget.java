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
      //System.out.println("Creating a widget of type " + componentName);
      PrologState props = context.getEngine().instantiateProps(getAttributes());
      PrologState initialState = new PrologState(context.getEngine().getInitialState(componentName, props.getValue()));
      PrologDocument userComponent = context.getEngine().render(componentName, initialState, props, context);
      //System.out.println("User component created the following document: " + userComponent);
      if (userComponent == null)
      {
         System.out.println("Unhandled type: " + this);
         System.exit(-1);
      }
      ReactComponent component = ReactComponentFactory.instantiateNode(userComponent, userComponent.getContext());
      widgetContext = userComponent.getContext();
      widgetContext.setRoot(component);
      //System.out.println("Registering a new flux listener for " + componentName);
      FluxDispatcher.registerFluxListener(componentName, widgetContext);
      return component;
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
