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

public class Field extends ReactLeafComponent
{
   private static final int TEXT = 0;
   private static final int RADIO = 1;
   private static final int CHECKBOX = 2;
   private static final int PASSWORD = 3;

   private InputWidget widget;
   private int type = TEXT;
   public Field()
   {
      widget = new TextField();
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
         widget.getAWTComponent().removeFocusListener(focusListener);
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
      widget.getAWTComponent().addFocusListener(focusListener);
   }




   public void setProperties(HashMap<String, PrologObject> properties)
   {
      if (properties.containsKey("fill"))
         fill = properties.get("fill").asFill();
      if (properties.containsKey("type"))
      {
         int oldType = type;
         String key = properties.get("type").asString();
         if (key == null)
            type = TEXT;
         else if (key.equals("text"))
            type = TEXT;
         else if (key.equals("radio"))
            type = RADIO;
         else if (key.equals("checkbox"))
            type = CHECKBOX;
         else if (key.equals("password"))
            type = PASSWORD;
         if (oldType != type)
         {
            switch(type)
            {
               case TEXT:
                  widget = new TextField();
                  break;
               case PASSWORD:
                  widget = new PasswordField();
                  break;
               case RADIO:
                  widget = new RadioButton();
                  break;
               case CHECKBOX:
                  widget = new CheckBox();
                  break;
            }
            if (getParentNode() != null)
               getParentNode().replaceChild(this, this);
         }
      }
      if (properties.containsKey("value"))
      {
         if (properties.get("value") == null)
            setValue(null);
         else
            setValue(properties.get("value"));
      }
      if (properties.containsKey("onBlur"))
         setFocusListener(properties.get("onBlur"));
      if (properties.containsKey("verifyValue"))
      {
         PrologObject handler = properties.get("verifyValue");
         if (handler == null || handler.isNull())
            widget.setVerifier(null);
         else
            widget.setVerifier(new InputWidgetVerifier()
               {
                  public boolean verifyValue(PrologObject newValue)
                  {
                     try
                     {
                        return getOwnerDocument().triggerEvent(handler.asTerm(), newValue.asTerm());
                     }
                     catch(Exception e)
                     {
                        e.printStackTrace();
                     }
                     return false;
                  }
               });
      }


      if (properties.containsKey("onChange"))
      {
         PrologObject handler = properties.get("onChange");
         if (handler == null || handler.isNull())
            widget.setChangeListener(null);
         else
            widget.setChangeListener(new InputWidgetListener()
               {
                  public void valueWouldChange(PrologObject newValue)
                  {
                     try
                     {
                        getOwnerDocument().triggerEvent(handler.asTerm(), newValue.asTerm());
                     }
                     catch(Exception e)
                     {
                        e.printStackTrace();
                     }
                  }
               });
      }

   }

   public void setValue(PrologObject value)
   {
      widget.setValue(value);
   }

   public Object getValue()
   {
      return widget.getValue();
   }

   public Component getAWTComponent()
   {
      return widget.getAWTComponent();
   }
}
