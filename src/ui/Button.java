package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;

import javax.swing.JButton;
import java.util.List;
import java.util.HashMap;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.Component;
import javax.swing.SwingConstants;

public class Button extends ReactLeafComponent 
{
   private JButton button = new JButton();

   public Button()
   {
      button.setHorizontalAlignment(SwingConstants.CENTER);
   }

   private PrologObject serializeObject()
   {
      return PrologObject.serialize(new HashMap<String, Object>());
   }

   private ActionListener actionListener = null;
   public void setClickHandler(PrologObject value)
   {
      if (actionListener != null)
         button.removeActionListener(actionListener);

      if (value == null || value.isNull())
         return;
      
      actionListener = new ActionListener()
         {
            public void actionPerformed(ActionEvent ae)
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
         };
      button.addActionListener(actionListener);
   }
   
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
      if (properties.containsKey("label"))
         button.setText(properties.get("label").asString());
      if (properties.containsKey("onClick"))
         setClickHandler(properties.get("onClick"));
      if (properties.containsKey("disabled"))
          button.setEnabled(!properties.get("disabled").asBoolean());
   }

   public Component getAWTComponent()
   {
      return button;
   }

   public String toString()
   {
      return "<Button " + button.getText() + ">";
   }
}
