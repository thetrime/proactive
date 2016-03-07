package org.proactive.ui;

import javax.swing.JFrame;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;

public class Frame extends ReactLeafComponent 
{
   JFrame frame = new JFrame();
   public void setProperties(HashMap<String, PrologObject> properties)
   {
   }
   public Component getAWTComponent()
   {
      return frame;
   }
}
