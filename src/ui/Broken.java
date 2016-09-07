package org.proactive.ui;

import javax.swing.JPanel;
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
   JPanel instance = new JPanel();

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
