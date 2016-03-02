package org.proactive.ui;

import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;

import javax.swing.JButton;
import java.util.List;
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
   
   public void setProperty(String name, PrologObject value)
   {
      if (name.equals("label"))
         button.setText(value.asString());
      else if (name.equals("fill"))
         fill = value.asFill();
      else if (name.equals("onClick"))
         setClickHandler(value);
   }

   public Component getAWTComponent()
   {
      return button;
   }
}
