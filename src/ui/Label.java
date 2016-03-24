package org.proactive.ui;

import javax.swing.JLabel;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;
import org.proactive.React;
import java.awt.Color;
import java.awt.Font;

public class Label extends ReactLeafComponent 
{
   JLabel label = new JLabel();

   private void setStyles()
   {
      Color colour = (Color)React.getStyle(id, className, "label", "colour");
      if (colour != null) label.setForeground(colour);
      Integer size = (Integer)React.getStyle(id, className, "label", "font-size");
      if (size != null) label.setFont(new Font(label.getFont().getName(), label.getFont().getStyle(), size.intValue()));


   }

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
      setStyles();
   }
   public Component getAWTComponent()
   {
      return label;
   }

   public String toString()
   {
      return "<Label: " + label.getText() + ">";
   }
}
