package org.proactive.ui;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.lang.reflect.Constructor;
import org.proactive.ReactComponent;
import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
import org.proactive.ReactComponentFactoryConfiguration;

public class DefaultReactComponentFactoryConfiguration implements ReactComponentFactoryConfiguration
{
   private HashMap<String, Constructor<? extends ReactComponent>> constructorHash = new HashMap<String, Constructor<? extends ReactComponent>>();
   public DefaultReactComponentFactoryConfiguration()
   {
      try
      {
         constructorHash.put("Panel", Panel.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("Field", Field.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("Button", Button.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("Title", Title.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("Label", Label.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("Table", Table.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("List", List.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("TextArea", TextArea.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("Tree", Tree.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("EditorPane", EditorPane.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("TabbedPane", TabbedPane.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("Frame", Frame.class.getConstructor(PrologNode.class, PrologContext.class));
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
   }

   public Constructor<? extends ReactComponent> getImplementingClass(String key)
   {
      return constructorHash.get(key);
   }
}
