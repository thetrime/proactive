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
   ReactListSelectionModel selectionModel;
   public List()
   {
      model = new ReactListModel<ListItem>();
      list = new JList<ListItem>(model);
      selectionModel = new ReactListSelectionModel();
      list.setSelectionModel(selectionModel);
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
      public void reallyAddSelectionInterval(int index0, int index1)
      {
         super.addSelectionInterval(index0, index1);
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
         ListItem item = (ListItem)child;
         if (sibling == null)
         {
            model.addElement(item);
            if (item.isSelected())
               selectionModel.reallyAddSelectionInterval(model.getSize()-1, model.getSize()-1);
         }
         else
         {
            int index = children.indexOf(sibling);
            model.insertElementAt(item, index);
            if (item.isSelected())
               selectionModel.reallyAddSelectionInterval(index, index);

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
