package org.proactive.ui;

import java.util.List;
import java.util.LinkedList;
import java.util.HashMap;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.ReactComponent;
import gnu.prolog.vm.TermConstants;

public class Row extends ReactComponent
{
   public Component getAWTComponent()
   {
      return null;
   }

   public Row()
   {
   }

   private List<RowChangeListener> listeners = new LinkedList<RowChangeListener>();
   public void addRowChangeListener(RowChangeListener listener)
   {
      listeners.add(listener);
   }

   public void removeRowChangeListener(RowChangeListener listener)
   {
      listeners.remove(listener);
   }

   private void rowUpdated()
   {
      for (RowChangeListener listener : listeners)
	 listener.rowChanged(this);
   }

   public void childUpdated(ReactComponent cell)
   {
      rowUpdated();
      // No need to propagate?
   }

   private interface DoubleClickHandler
   {
      public void doubleClick();
   }

   private DoubleClickHandler doubleClickHandler = null;
   private void setDoubleClickHandler(PrologObject value)
   {
      if (value.isNull())
	 doubleClickHandler = null;
      else
      {
	 doubleClickHandler = new DoubleClickHandler()
	    {
	       public void doubleClick()
	       {
		  try
		  {
		     getOwnerDocument().triggerEvent(value.asTerm(), TermConstants.emptyListAtom);
		  }
		  catch (Exception e)
		  {
		     e.printStackTrace();
		  }
	       }
	    };
      }
   }

   public void doubleClick()
   {
      if (doubleClickHandler != null)
	 doubleClickHandler.doubleClick();
   }

   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
      if (properties.containsKey("onDoubleClick"))
	 setDoubleClickHandler(properties.get("onDoubleClick"));
   }  

   public ReactComponent get(int index)
   {
      return index<children.size()?children.get(index):null;
   }

   public int getSize()
   {
      return children.size();
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      super.insertChildBefore(child, sibling);
      rowUpdated();
   }

   public void removeChild(ReactComponent child)
   {
      super.removeChild(child);
      rowUpdated();
   }

   public void replaceChild(ReactComponent oldChild, ReactComponent newChild)
   {
      super.replaceChild(oldChild, newChild);
      rowUpdated();
   }

}
