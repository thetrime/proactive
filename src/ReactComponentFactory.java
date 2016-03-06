package org.proactive;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.lang.reflect.Constructor;
import org.proactive.vdom.PrologNode;
import org.proactive.vdom.PrologDocument;
import org.proactive.vdom.PrologWidget;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologState;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.FluxDispatcher;
import org.proactive.prolog.Engine;
import gnu.prolog.term.Term;

public class ReactComponentFactory
{
    private static ReactComponentFactoryConfiguration configuration = new org.proactive.ui.DefaultReactComponentFactoryConfiguration();
    public static void setUIConfiguration(ReactComponentFactoryConfiguration c)
    {
        configuration = c;
    }   

   public static ReactComponent instantiateNode(PrologNode n, PrologContext context) throws Exception
   {
      if (n instanceof PrologWidget)
      {
         return ((PrologWidget)n).init(context);
      }
      try
      {
         Constructor<? extends ReactComponent> c = configuration.getImplementingClass(n.getNodeName());
         if (c != null)
         {
            ReactComponent instance = c.newInstance(n, context);
            if (context != null)
               context.setRoot(instance);
            applyNodeAttributes(n, instance, context);
            addNodeChildren(n, instance, context);
            return instance;
         }
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
      System.out.println("Could not instantiate node from "+ n);
      System.exit(-1);
      return null;
   }

   private static void addNodeChildren(PrologNode n, ReactComponent target, PrologContext context) throws Exception
   {
      for (PrologNode child : n.getChildren())
      {
         ReactComponent component = instantiateNode(child, context);
         target.insertChildBefore(component, null);
      }

   }

   private static void applyNodeAttributes(PrologNode n, ReactComponent target, PrologContext context)
   {
      HashMap<String, PrologObject> attributes = new HashMap<String, PrologObject>();
      for (Map.Entry<String, Term> entry : n.getAttributes().entrySet())
      {
         Term t = Engine.unpack(entry.getValue(), context.getParentContext());
         attributes.put(entry.getKey(), new PrologObject(t));
      }
      target.setProperties(attributes);
   }

   public static boolean isWidgetName(String name)
   {
      return (configuration.getImplementingClass(name) == null);
   }



   // Redo
   public static ReactComponent createElement(String tagName) throws Exception
   {
      Constructor<? extends ReactComponent> c = configuration.getImplementingClass(tagName);
      if (c != null)
         return c.newInstance();
      return null;
   }


}
