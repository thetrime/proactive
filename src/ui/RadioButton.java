package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import java.awt.Component;
import java.awt.BorderLayout;
import javax.swing.JRadioButton;
import javax.swing.JPanel;

public class RadioButton implements InputWidget
{
   JPanel panel = new JPanel();
   JRadioButton field = new JRadioButton();
   public RadioButton()
   {
      panel.setLayout(new BorderLayout());
      panel.setOpaque(false);
      panel.add(field, BorderLayout.EAST);
   }

   public Component getAWTComponent()
   {
      return panel;
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

   public void setDisabled(boolean disabled)
   {
      field.setEnabled(!disabled);
   }


   public void setChangeListener(InputWidgetListener value) {}
   public void setVerifier(InputWidgetVerifier value) {}
}
