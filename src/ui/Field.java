package org.proactive.ui;

import javax.swing.JTextField;
import javax.swing.JPanel;
import javax.swing.JLabel;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import java.awt.BorderLayout;
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
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      if (properties.containsKey("fill"))
         fill = properties.get("fill").asFill();
   }
   public Component getAWTComponent()
   {
      return field;
   }
}
