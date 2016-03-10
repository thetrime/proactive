package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import java.awt.Component;
import javax.swing.JComboBox;
import javax.swing.text.DocumentFilter;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import java.util.HashMap;
import java.util.List;
import javax.swing.InputVerifier;
import javax.swing.JComponent;

public class ComboBox implements InputWidget
{
   JComboBox<ComboItem> field = null;
   private DocumentFilter documentFilter = null;
   private DocumentFilter.FilterBypass bypass = null;
   public ComboBox()
   {
      field = new JComboBox<ComboItem>();
   }

   public Component getAWTComponent()
   {
      return field;
   }

   public Object getValue()
   {
      Object value = null;

      if (field.getSelectedIndex() != -1)
         value = ((ComboItem)field.getItemAt(field.getSelectedIndex())).getValue();

      System.out.println("GetValue: " + value);
      return value;
   }

   public void setValue(PrologObject value)
   {
      ComboItem comboItem = new ComboItem(value.asString());
      field.setSelectedItem(comboItem);
   }

   public void setAllowedValues(PrologObject values)
   {
      List<PrologObject> allowedValues = values.asList();
      field.removeAllItems();
      for (PrologObject object: allowedValues)
         field.addItem(new ComboItem(object.asNameValuePair()));
   }

   public class ComboItem
   {
      String name;
      PrologObject value;
      public ComboItem(PrologObject.NameValuePair pair)
      {
         name = pair.getKey();
         value = pair.getValue();
      }
      public ComboItem(String key)
      {
         this.name = key;
         this.value = null;
      }
      public PrologObject getValue()
      {
         return value;
      }

      @Override
      public String toString()
      {
         return name;
      }
      @Override
      public boolean equals(Object o)
      {
         return (o instanceof ComboItem) && (((ComboItem)o).name.equals(name));
      }
   }

   public void setVerifier(InputWidgetVerifier listener) {}


   public void setChangeListener(InputWidgetListener listener)
   {
      // FIXME: Implement this
   }

   public void setDisabled(boolean disabled)
   {
      field.setEnabled(!disabled);
   }

}
