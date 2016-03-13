package org.proactive.ui;

import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.ReactComponent;

public class Row extends ReactComponent
{
   public Component getAWTComponent()
   {
      return null;
   }

   public Row()
   {
   }

   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
   }  

   public ReactComponent get(int index)
   {
      return index<children.size()?children.get(index):null;
   }
}
