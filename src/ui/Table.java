package org.proactive.ui;

import javax.swing.JTable;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;

public class Table extends ReactLeafComponent 
{
   JTable table = new JTable();
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      if (properties.containsKey("fill"))
         fill = properties.get("fill").asFill();
   }
   public Component getAWTComponent()
   {
      return table;
   }
}
