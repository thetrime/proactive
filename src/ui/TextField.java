package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import java.awt.Component;
import javax.swing.JTextField;

public class TextField implements InputWidget
{
   JTextField field = new JTextField();
   public TextField()
   {
   }

   public Component getAWTComponent()
   {
      return field;
   }

   public Object getValue()
   {
      return field.getText();
   }

   public void setValue(PrologObject value)
   {
      if (value == null)
         field.setText("");
      else
         field.setText(value.asString());
   }
}
