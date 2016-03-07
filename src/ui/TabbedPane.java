package org.proactive.ui;

import javax.swing.JTabbedPane;
import javax.swing.JFrame;
import java.util.List;
import java.util.HashMap;
import java.util.LinkedList;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactComponent;
import org.proactive.ui.Tab;

public class TabbedPane extends ReactComponent
{
   int nextIndex = 0;
   JTabbedPane tabbedPane = new JTabbedPane();
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
   }
   public Component getAWTComponent()
   {
      return tabbedPane;
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      // First insert the visual component
      int index = (sibling==null)?-1:children.indexOf(sibling);
      super.insertChildBefore(child, sibling);
      if (index != -1)
      {
         repackChildren();
      }
      else
      {
         addChildToDOM(nextIndex, child);
         nextIndex++;         
      }
   }

   private void addChildToDOM(int index, ReactComponent child)
   {
      if (child instanceof Tab)
      {
         Tab childTab = (Tab)child;
         tabbedPane.insertTab(childTab.getLabel(), null, child.getAWTComponent(), childTab.getTooltip(), index);
      }
   }
   
   private void repackChildren()
   {
      tabbedPane.removeAll();
      nextIndex = 0;
      for (ReactComponent i : children)
         addChildToDOM(nextIndex++, i);
   }   

   public void removeChild(ReactComponent child)
   {
      tabbedPane.remove(awtMap.get(child));
      super.removeChild(child);
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      int i = children.indexOf(oldChild);
      children.set(i, newChild);
      tabbedPane.remove(oldChild.getAWTComponent());
      newChild.setParentNode(this);
      newChild.setOwnerDocument(owner);
      if (newChild instanceof Tab)
      {
         Tab childTab = (Tab)newChild;
         tabbedPane.insertTab(childTab.getLabel(), null, newChild.getAWTComponent(), childTab.getTooltip(), i);
      }
   }

   public List<ReactComponent> getChildNodes()
   {
      return children;
   }
}
