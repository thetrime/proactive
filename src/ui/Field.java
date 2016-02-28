package org.proactive.ui;

import javax.swing.JTextField;
import java.util.List;
import java.awt.Component;
import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
import org.proactive.ReactLeafComponent;

public class Field extends ReactLeafComponent 
{
   private JTextField field = new JTextField("A field");
   public Field(PrologNode n, PrologContext context)
   {
      super(context);
   }
   public void setProperty(String name, Object value)
   {
      if (name.equals("fill"))
         fill = context.getFill(value);
   }
   public Component getAWTComponent()
   {
      return field;
   }
}
