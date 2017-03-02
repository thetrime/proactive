package org.proactive.ui;

import javax.swing.JLabel;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;
import org.proactive.React;
import java.awt.Color;
import java.awt.Font;

public class Broken extends ReactLeafComponent 
{
   JLabel instance = new JLabel();
   public Broken(String name)
   {
      instance.setText("Broken-" + name);
      // If you prefer to have a red, possible invisible box, try changing instance to JPanel and use one or both of the following:
//      instance.setBackground(Color.RED);
//      instance.setPreferredSize(new java.awt.Dimension(0,0));
   }
   public void setProperties(HashMap<String,PrologObject> properties)
   {
      super.setProperties(properties);
   }
   public Component getAWTComponent()
   {
      return instance;
   }

   public String toString()
   {
      return "<Broken>";
   }
}
