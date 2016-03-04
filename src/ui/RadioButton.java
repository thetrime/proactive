package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import java.awt.Component;
import javax.swing.JRadioButton;

public class RadioButton implements InputWidget
{
   JRadioButton field = new JRadioButton();
   public RadioButton()
   {
   }

   public Component getAWTComponent()
   {
      return field;
   }

   public Object getValue()
   {
      return field.isSelected();
   }

   public void setValue(PrologObject value)
   {
      if (value == null)
         field.setSelected(false);
      else
         field.setSelected(value.asBoolean());
   }
}
