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
   public Label(PrologNode n, PrologContext context)
   {
      super(context);
      /* This is if the child is the label.
        Node child = n.getFirstChild();
        if (child != null && child instanceof Text)
        setText(((Text)child).getWholeText());
      */
   }
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      if (properties.containsKey("label"))
      {
         PrologObject value = properties.get("label");
         if (value == null)
            label.setText("");
         else
            label.setText(value.asString());
      }
      if (properties.containsKey("fill"))
         fill = properties.get("fill").asFill();
   }
   public Component getAWTComponent()
   {
      return label;
   }
}
