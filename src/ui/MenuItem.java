package org.proactive.ui;

import javax.swing.JMenuItem;
import java.util.List;
import java.util.HashMap;
import java.util.LinkedList;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactComponent;

public class MenuItem extends ReactComponent
{
   protected JMenuItem item = new JMenuItem();
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
      if (properties.containsKey("label"))
         item.setText(properties.get("label").asString());
      if (properties.containsKey("onClick"))
         setClickHandler(properties.get("onClick"));

   }
   public Component getAWTComponent()
   {
      return item;
   }

   private ActionListener actionListener = null;
   public void setClickHandler(PrologObject value)
   {
      if (actionListener != null)
         item.removeActionListener(actionListener);

      if (value == null || value.isNull())
         return;
      
      actionListener = new ActionListener()
         {
            public void actionPerformed(ActionEvent ae)
            {
               try
               {
                  getOwnerDocument().triggerEvent(value.asTerm(), serializeObject().asTerm());
               }
               catch (Exception e)
               {
                  e.printStackTrace();
               }
            }
         };
      item.addActionListener(actionListener);
   }

   private PrologObject serializeObject()
   {
      return PrologObject.serialize(new HashMap<String, Object>());
   }

   /* Not recommended!! For now we will accept them but ignore them
   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      int index = (sibling==null)?-1:children.indexOf(sibling);
      super.insertChildBefore(child, sibling);
      if (index == -1)
         menu.add(child.getAWTComponent());
      else
         menu.insert(child.getAWTComponent(), index);
   }

   public void removeChild(ReactComponent child)
   {
      menu.remove(awtMap.get(child));
      super.removeChild(child);
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      int index = children.indexOf(oldChild);
      menu.remove(awtMap.get(oldChild));
      menu.insert(newChild.getAWTComponent(), index);
      super.replaceChild(newChild, oldChild);
   }
   */

}
