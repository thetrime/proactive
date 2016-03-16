package org.proactive.ui;

import javax.swing.JTable;
import java.util.List;
import java.util.HashMap;
import java.util.LinkedList;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactComponent;

public class TableHeader extends ReactComponent
{
   public Component getAWTComponent()
   {
      return null;
   }

   public int getSize()
   {
      return children.size();
   }

   public ReactComponent getColumnHeader(int index)
   {
      return index<children.size()?children.get(index):null;
   }

   public List<ReactComponent> getColumnHeaders()
   {
      return children;
   }

}
