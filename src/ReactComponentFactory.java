package org.proactive;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.lang.reflect.Constructor;
import org.proactive.vdom.PrologNode;
import org.proactive.vdom.PrologDocument;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologState;
import org.proactive.prolog.FluxDispatcher;

public class ReactComponentFactory
{
    private static ReactComponentFactoryConfiguration configuration = new org.proactive.ui.DefaultReactComponentFactoryConfiguration();
    public static void setUIConfiguration(ReactComponentFactoryConfiguration c)
    {
        configuration = c;
    }   

   public static ReactComponent instantiateNode(PrologNode n, PrologContext context) throws Exception
   {
      try
      {
         Constructor<? extends ReactComponent> c = configuration.getImplementingClass(n.getNodeName());
         if (c != null)
         {
            ReactComponent instance = c.newInstance(n, context);
            if (context != null)
               context.setRoot(instance);
            applyNodeAttributes(n, instance);
            return instance;
         }
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
      // User-defined component
      System.out.println(" ### Creating user-defined type " + n.getNodeName());
      PrologState props = context.getEngine().instantiateProps(n.getAttributes());
      PrologState initialState = context.getEngine().getInitialState(n.getNodeName(), props);
      PrologDocument userComponent = context.getEngine().render(n.getNodeName(), initialState, props);
      System.out.println("User component created the following document: " + userComponent);
      if (userComponent == null)
      {
         System.out.println("Unhandled type: " + n);
         System.exit(-1);
      }
      ReactComponent component = instantiateNode(userComponent, userComponent.getContext());
      userComponent.getContext().setRoot(component);
      System.out.println("Registering a new flux listener for " + n.getNodeName());
      FluxDispatcher.registerFluxListener(n.getNodeName(), userComponent.getContext());               
      return component;
   }
   
   private static void applyNodeAttributes(PrologNode n, ReactComponent target)
   {
      Map<String, Object> attributes = n.getAttributes();
      for (Iterator<Map.Entry<String, Object>> i = attributes.entrySet().iterator(); i.hasNext();)
      {
         Map.Entry<String, Object> entry = i.next();
         target.setProperty(entry.getKey(), entry.getValue());
      }
   }
}
