package org.proactive.ui;

import javax.swing.JTextField;
import javax.swing.JPanel;
import javax.swing.JLabel;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import java.awt.BorderLayout;
import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;
import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import org.proactive.ReactLeafComponent;

public class Field extends ReactLeafComponent 
{
   private JTextField field = new JTextField();
   public Field(PrologNode n, PrologContext context)
   {
      super(context);
   }

   private PrologObject serializeObject()
   {
      HashMap<String, Object> properties = new HashMap<String, Object>();
      properties.put("value", field.getText());
      return PrologObject.serialize(properties);
   }

   private FocusListener focusListener = null;
   private void setFocusListener(PrologObject value)
   {
      if (focusListener != null)
         field.removeFocusListener(focusListener);
      if (value == null)
         return;
      focusListener = new FocusListener()
         {
            public void focusLost(FocusEvent fe)
            {
               try
               {
                  context.triggerEvent(value.asTerm(), serializeObject());
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


   public void setProperties(HashMap<String, PrologObject> properties)
   {
      if (properties.containsKey("fill"))
         fill = properties.get("fill").asFill();
      if (properties.containsKey("value"))
      {
         System.out.println("Set value to " + properties.get("value").asTerm());
         if (properties.get("value") == null)
            field.setText("");
         else
            field.setText(properties.get("value").asString());
      }
      if (properties.containsKey("onBlur"))
         setFocusListener(properties.get("onBlur"));
   }
   public Component getAWTComponent()
   {
      return field;
   }
}
