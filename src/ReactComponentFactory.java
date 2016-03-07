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

   public static ReactComponent instantiateNode(PrologNode n, ReactWidget context) throws Exception
   {
      System.out.println("Could not instantiate node from "+ n);
      System.exit(-1);
      return null;
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
