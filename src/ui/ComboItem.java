package org.proactive.ui;

import javax.swing.JTextField;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JCheckBox;
import javax.swing.JPasswordField;
import javax.swing.BorderFactory;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import java.awt.BorderLayout;
import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;
import org.proactive.prolog.PrologObject;
import org.proactive.ReactLeafComponent;

public class ComboItem extends ReactLeafComponent
{
   private String label = "";
   private PrologObject value = null;
   public Component getAWTComponent()
   {
      return null;
   }

   public ComboItem()
   {
   }

   // Used by ComboBox to create a fake key to set the selected item
   public ComboItem(PrologObject value)
   {
      this.value = value;
   }

   public void setProperties(HashMap<String, PrologObject> properties)
   {
      if (properties.containsKey("label"))
         label = properties.get("label").asString();
      if (properties.containsKey("value"))
         value = properties.get("value");
   }

   public String toString()
   {
      return label;
   }

   public boolean equals(Object o)
   {
      return (o instanceof ComboItem) && (((ComboItem)o).value.asTerm().equals(value.asTerm()));
   }

   public PrologObject getValue()
   {
      return value;
   }
}
