package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import java.awt.Component;
import javax.swing.JCheckBox;

public class CheckBox implements InputWidget
{
   JCheckBox field = new JCheckBox();
   public CheckBox()
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
