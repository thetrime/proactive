package org.proactive.ui;

import javax.swing.JTextField;
import javax.swing.JPanel;
import javax.swing.JLabel;
import java.util.List;
import java.awt.Component;
import java.awt.BorderLayout;
import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import org.proactive.ReactLeafComponent;

public class Field extends ReactLeafComponent 
{
   private JTextField field = new JTextField();
   private JPanel panel = new JPanel();
   private JLabel label = null;
   public Field(PrologNode n, PrologContext context)
   {
      super(context);
      panel.setLayout(new BorderLayout());
      panel.add(field, BorderLayout.CENTER);
   }
   public void setProperty(String name, PrologObject value)
   {
      if (name.equals("fill"))
         fill = value.asFill();
      if (name.equals("label"))
      {
         if (value == null)
         {
            if (label != null)
               panel.remove(label);
            label = null;
         }
         else
         {
            if (label == null)
            {
               label = new JLabel();
               panel.add(label, BorderLayout.WEST);
            }
            label.setText(value.asString());
         }
      }

   }
   public Component getAWTComponent()
   {
      return panel;
   }
}
