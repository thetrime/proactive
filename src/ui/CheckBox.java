package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import java.awt.Component;
import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.JCheckBox;
import java.util.HashMap;


public class CheckBox implements InputWidget
{
   JPanel panel = new JPanel();
   JCheckBox field = new JCheckBox();
   private InputWidgetListener listener = null;
   public CheckBox()
   {
      panel.setLayout(new BorderLayout());
      panel.setOpaque(false);
      panel.add(field, BorderLayout.EAST);
      field.setModel(new ReactButtonModel());
   }

   public class ReactButtonModel extends JToggleButton.ToggleButtonModel
   {
      public void setSelected(boolean isSelected)
      {
         if (listener != null)
         {
            try
            {
               HashMap<String, Object> properties = new HashMap<String, Object>();
               properties.put("value", isSelected);
               listener.valueWouldChange(PrologObject.serialize(properties));
            }
            catch(Exception e)
            {
               e.printStackTrace();
            }
         }
      }
      public void reallySetSelected(boolean isSelected)
      {
         super.setSelected(isSelected);
      }
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
         ((ReactButtonModel)field.getModel()).reallySetSelected(false);
      else
         ((ReactButtonModel)field.getModel()).reallySetSelected(value.asBoolean());
   }

   public void setDisabled(boolean disabled)
   {
      field.setEnabled(!disabled);
   }

   public void setChangeListener(InputWidgetListener value)
   {
      this.listener = value;
   }
   public void setVerifier(InputWidgetVerifier value) {}
}
