package org.proactive.ui;

import javax.swing.JList;
import java.awt.Component;
import java.util.HashMap;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;

public class List extends ReactLeafComponent 
{
   JList list = new JList();
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      if (properties.containsKey("fill"))
         fill = properties.get("fill").asFill();
   }
   public Component getAWTComponent()
   {
      return list;
   }
}
