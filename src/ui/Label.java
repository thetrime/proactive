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
   public void setProperty(String name, PrologObject value)
   {
      super.setProperty(name, value);
      if (name.equals("label"))
      {
         if (value == null)
            label.setText("");
         else
            label.setText(value.asString());
      }
   }
   public Component getAWTComponent()
   {
      return label;
   }
}
