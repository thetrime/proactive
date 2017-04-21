package org.proactive.ui;

import java.util.List;
import java.util.LinkedList;
import java.util.HashMap;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.ReactComponent;
import gnu.prolog.vm.TermConstants;

public class TableFooter extends ReactComponent
{
   public Component getAWTComponent()
   {
      return null;
   }

   public TableFooter()
   {
   }

   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
   }

   public int getRowCount()
   {
      return children.size();
   }

   public ReactComponent getRow(int index)
   {
      return index<children.size()?children.get(index):null;
   }

}
