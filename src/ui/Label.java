package org.proactive.ui;

import javax.swing.JLabel;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;

public class Label extends ReactLeafComponent 
{
   JLabel label = new JLabel();
   public void setProperties(HashMap<String,PrologObject> properties)
   {
      super.setProperties(properties);
      if (properties.containsKey("label"))
      {
         if (properties.get("label") == null)
            label.setText("");
         else
            label.setText(properties.get("label").asString());
      }
   }
   public Component getAWTComponent()
   {
      return label;
   }
}
