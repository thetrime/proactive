package org.proactive;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.lang.reflect.Constructor;
import org.proactive.vdom.PrologNode;
import org.proactive.vdom.PrologDocument;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologState;

public class ReactComponentFactory
{
    private static ReactComponentFactoryConfiguration configuration;
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
      PrologState initialState = context.getEngine().getInitialState(n.getNodeName());
      PrologDocument userComponent = context.getEngine().render(n.getNodeName(), initialState, context.getEngine().instantiateProps(n.getAttributes()));
      if (userComponent == null)
      {
         System.out.println("Unhandled type: " + n);
         System.exit(-1);
      }
      return instantiateNode(userComponent, userComponent.getContext());
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
