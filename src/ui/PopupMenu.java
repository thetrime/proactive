package org.proactive.ui;

import javax.swing.JPopupMenu;
import java.util.List;
import java.util.HashMap;
import java.util.LinkedList;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactComponent;

public class PopupMenu extends ReactComponent
{
   protected JPopupMenu menu = new JPopupMenu();
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
   }
   public Component getAWTComponent()
   {
      return menu;
   }

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

   public JPopupMenu getMenu()
   {
      return menu;
   }
}
