package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import java.awt.Component;
import javax.swing.JPasswordField;

public class PasswordField implements InputWidget
{
   JPasswordField field = new JPasswordField();
   public PasswordField()
   {
   }

   public Component getAWTComponent()
   {
      return field;
   }

   public Object getValue()
   {
      return field.getPassword();
   }

   public void setValue(PrologObject value)
   {
      if (value == null)
         field.setText("");
      else
         field.setText(value.asString());
   }

   public void setDisabled(boolean disabled)
   {
      field.setEnabled(!disabled);
   }


   public void setChangeListener(InputWidgetListener value) {}
   public void setVerifier(InputWidgetVerifier value) {}


}
