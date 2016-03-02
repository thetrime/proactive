package org.proactive.ui;

import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;

import javax.swing.JButton;
import java.util.List;
import java.util.HashMap;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.Component;

public class Button extends ReactLeafComponent 
{
   private JButton button = new JButton();
   public Button(PrologNode n, PrologContext context)
   {
      super(context);
   }
   
   private ActionListener actionListener = null;
   public void setClickHandler(PrologObject value)
   {
      if (actionListener != null)
         button.removeActionListener(actionListener);

      if (value == null)
         return;
      
      actionListener = new ActionListener()
         {
            public void actionPerformed(ActionEvent ae)
            {
               try
               {
                  context.triggerEvent(value.asTerm());
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
      if (properties.containsKey("label"))
         button.setText(properties.get("label").asString());
      if (properties.containsKey("fill"))
         fill = properties.get("fill").asFill();
      if (properties.containsKey("onClick"))
         setClickHandler(properties.get("onClick"));
   }

   public Component getAWTComponent()
   {
      return button;
   }
}
