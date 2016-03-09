package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import java.awt.Component;
import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.JCheckBox;

public class CheckBox implements InputWidget
{
   JPanel panel = new JPanel();
   JCheckBox field = new JCheckBox();
   public CheckBox()
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

   public void setChangeListener(InputWidgetListener value) {}
}
