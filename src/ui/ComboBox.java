package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import org.proactive.ReactComponent;
import java.awt.Component;
import javax.swing.JComboBox;
import javax.swing.DefaultComboBoxModel;
import javax.swing.text.DocumentFilter;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import java.util.HashMap;
import java.util.List;
import javax.swing.JComponent;
import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;

public class ComboBox extends ReactComponent
{
   JComboBox<ComboItem> field = null;
   private boolean stillConstructing = false;

   public ComboBox()
   {
      field = new JComboBox<ComboItem>(new ReactComboBoxModel());
   }
   public class ReactComboBoxModel extends DefaultComboBoxModel<ComboItem>
   {
      public void reallySetSelectedItem(Object anObject)
      {
         super.setSelectedItem(anObject);
      }

      @Override
      public void setSelectedItem(Object anObject)
      {
         if (stillConstructing)
            return;
         if (changeListener != null)
            changeListener.stateWouldChange(((ComboItem)anObject).getValue());
      }
   }

   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
      if (properties.containsKey("value"))
         setValue(properties.get("value"));
      if (properties.containsKey("onBlur"))
         setFocusListener(properties.get("onBlur"));
      if (properties.containsKey("disabled"))
         field.setEnabled(!properties.get("disabled").asBoolean());
      if (properties.containsKey("onChange"))
         setChangeListener(properties.get("onChange"));
   }

   private PrologObject serializeObject()
   {
      HashMap<String, Object> properties = new HashMap<String, Object>();
      properties.put("value", getValue());
      return PrologObject.serialize(properties);
   }

   private FocusListener focusListener = null;
   private void setFocusListener(PrologObject value)
   {
      if (focusListener != null)
         field.removeFocusListener(focusListener);
      if (value == null || value.isNull())
         return;
      focusListener = new FocusListener()
         {
            public void focusLost(FocusEvent fe)
            {
               try
               {
                  getOwnerDocument().triggerEvent(value.asTerm(), serializeObject().asTerm());
               }
               catch (Exception e)
               {
                  e.printStackTrace();
               }
            }
            public void focusGained(FocusEvent fe)
            {
            }
         };
      field.addFocusListener(focusListener);
   }

   protected interface ChangeListener
   {
      public void stateWouldChange(PrologObject newValue);
   }

   ChangeListener changeListener = null;
   public void setChangeListener(PrologObject value)
   {
      if (value == null || value.isNull())
         changeListener = null;
      changeListener = new ChangeListener()
         {
            public void stateWouldChange(PrologObject newValue)
            {
               try
               {
                  HashMap<String, Object> properties = new HashMap<String, Object>();
                  properties.put("value", newValue);
                  getOwnerDocument().triggerEvent(value.asTerm(), PrologObject.serialize(properties).asTerm());
               }
               catch (Exception e)
               {
                  e.printStackTrace();
               }
            }
         };
   }

   public Component getAWTComponent()
   {
      return field;
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      super.insertChildBefore(child, sibling);
      if (child instanceof ComboItem)
      {
         stillConstructing = true;
         try
         {
            if (sibling == null)
               field.addItem((ComboItem)child);
            else
            {
               int index = children.indexOf(sibling);
               field.insertItemAt((ComboItem)child, index);
            }
            if (child.equals(currentValue))
               ((ReactComboBoxModel)field.getModel()).reallySetSelectedItem((ComboItem)child);
         }
         finally
         {
            stillConstructing = false;
         }

      }
   }

   public void removeChild(ReactComponent child)
   {
         super.removeChild(child);
         if (child instanceof ComboItem)
         {
            stillConstructing = true;
            try
            {
               field.removeItem((ComboItem)child);
            }
            finally
            {
               stillConstructing = false;
            }
         }
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      if (newChild instanceof ComboItem)
      {
         stillConstructing = true;
         try
         {
            int index = children.indexOf(oldChild);
            field.removeItemAt(index);
            field.insertItemAt((ComboItem)newChild, index);
            if (newChild.equals(currentValue))
               ((ReactComboBoxModel)field.getModel()).reallySetSelectedItem(newChild);
         }
         finally
         {
            stillConstructing = false;
         }
      }

      super.replaceChild(newChild, oldChild);
   }

   public Object getValue()
   {
      Object value = null;

      if (field.getSelectedIndex() != -1)
         value = ((ComboItem)field.getItemAt(field.getSelectedIndex())).getValue();

      return value;
   }

   private ComboItem currentValue;
   public void setValue(PrologObject value)
   {
      currentValue = new ComboItem(value);
   }
}
