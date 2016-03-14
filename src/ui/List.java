package org.proactive.ui;

import javax.swing.JList;
import javax.swing.DefaultListModel;
import javax.swing.DefaultListSelectionModel;
import java.awt.Component;
import java.util.HashMap;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactComponent;

public class List extends ReactComponent
{
   JList<ListItem> list = null;
   ReactListModel<ListItem> model = null;
   
   public List()
   {
      model = new ReactListModel<ListItem>();
      list = new JList<ListItem>(model);
      list.setSelectionModel(new ReactListSelectionModel());
   }

   public class ReactListSelectionModel extends DefaultListSelectionModel
   {
      public void addSelectionInterval(int index0, int index1)
      {
         System.out.println("List would select");
      }
      public void setSelectionInterval(int index0, int index1)
      {
         System.out.println("List would select");
      }
   }

   public class ReactListModel<E> extends DefaultListModel<E>
   {
      
   }
      
   public void setProperties(HashMap<String, PrologObject> properties)
   {
     super.setProperties(properties);
   }
   public Component getAWTComponent()
   {
      return list;
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      super.insertChildBefore(child, sibling);
      if (child instanceof ListItem)
      {
         if (sibling == null)
            model.addElement((ListItem)child);
         else
         {
            int index = children.indexOf(sibling);
            model.insertElementAt((ListItem)child, index);
         }
      }
   }

   public void removeChild(ReactComponent child)
   {
      super.removeChild(child);
      if (child instanceof ListItem)
      {
         try
         {
            model.removeElement((ListItem)child);
         }
         finally
         {           
         }
      }
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      if (newChild instanceof ListItem)
      {
         try
         {
            int index = children.indexOf(oldChild);
            model.removeElementAt(index);
            model.insertElementAt((ListItem)newChild, index);
         }
         finally
         {
         }
      }
      super.replaceChild(newChild, oldChild);
   }
}
