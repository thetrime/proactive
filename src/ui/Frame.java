package org.proactive.ui;

import javax.swing.JFrame;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import java.awt.BorderLayout;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactComponent;

public class Frame extends ReactComponent 
{
   JFrame frame = new JFrame();
   Panel contentPane = new Panel();

   public Frame()
   {
      frame.getContentPane().setLayout(new BorderLayout());
      frame.getContentPane().add(contentPane.getAWTComponent(), BorderLayout.CENTER);
   }
   
   public void setProperties(HashMap<String, PrologObject> properties)
   {
     super.setProperties(properties);
     if (properties.containsKey("visible"))
     {
       frame.setVisible(properties.get("visible").asBoolean());
     }
   }
   public Component getAWTComponent()
   {
      return frame;
   }

  public void insertChildBefore(ReactComponent child, ReactComponent sibling)
  {
     super.insertChildBefore(child, sibling);
     contentPane.insertChildBefore(child, sibling);
  }

  public void removeChild(ReactComponent child)
  {
     super.removeChild(child);
     contentPane.removeChild(child);
  }

  public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
  {
     super.replaceChild(newChild, oldChild);
     contentPane.replaceChild(newChild, oldChild);
  }
}
