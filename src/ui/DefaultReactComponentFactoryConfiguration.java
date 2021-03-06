package org.proactive.ui;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.lang.reflect.Constructor;
import org.proactive.ReactComponent;
import org.proactive.ReactComponentFactoryConfiguration;

public class DefaultReactComponentFactoryConfiguration implements ReactComponentFactoryConfiguration
{
   private HashMap<String, Constructor<? extends ReactComponent>> constructorHash = new HashMap<String, Constructor<? extends ReactComponent>>();
   public DefaultReactComponentFactoryConfiguration()
   {
      try
      {
         constructorHash.put("Panel", Panel.class.getConstructor());
         constructorHash.put("Field", Field.class.getConstructor());
         constructorHash.put("ComboBox", ComboBox.class.getConstructor());
         constructorHash.put("ComboItem", ComboItem.class.getConstructor());
         constructorHash.put("Button", Button.class.getConstructor());
         constructorHash.put("Label", Label.class.getConstructor());
         constructorHash.put("Table", Table.class.getConstructor());
         constructorHash.put("TableHeader", TableHeader.class.getConstructor());
         constructorHash.put("TableFooter", TableFooter.class.getConstructor());
         constructorHash.put("Row", Row.class.getConstructor());
         constructorHash.put("List", List.class.getConstructor());
         constructorHash.put("ListItem", ListItem.class.getConstructor());
         constructorHash.put("TextArea", TextArea.class.getConstructor());
         constructorHash.put("Tree", Tree.class.getConstructor());
         constructorHash.put("TreeNode", TreeNode.class.getConstructor());
         constructorHash.put("EditorPane", EditorPane.class.getConstructor());
         constructorHash.put("TabbedPane", TabbedPane.class.getConstructor());
         constructorHash.put("Tab", Tab.class.getConstructor());
         constructorHash.put("Frame", Frame.class.getConstructor());
         constructorHash.put("PopupMenu", PopupMenu.class.getConstructor());
         constructorHash.put("MenuItem", MenuItem.class.getConstructor());
         constructorHash.put("Grid", Grid.class.getConstructor());
         constructorHash.put("Image", Image.class.getConstructor());
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

   public void registerComponent(String key, Constructor<? extends ReactComponent> constructor)
   {
      constructorHash.put(key, constructor);
   }
}
