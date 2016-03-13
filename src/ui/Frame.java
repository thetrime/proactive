package org.proactive.ui;

import javax.swing.JFrame;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.BorderLayout;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactComponent;

public class Frame extends ReactComponent 
{
   protected JFrame frame = new JFrame();
   private Panel contentPane = new Panel();
   private boolean userHasResizedFrame = false;
   private int repackCount = 0;
   public Frame()
   {
      frame.getContentPane().setLayout(new BorderLayout());
      frame.getContentPane().add(contentPane.getAWTComponent(), BorderLayout.CENTER);
      frame.pack();
      frame.addComponentListener(new ComponentAdapter()
         {
            public void componentResized(ComponentEvent e)
            {
               if (repackCount == 0)
                  userHasResizedFrame = true;
               else if (repackCount > 0)
                  repackCount--;
            }
         });
      frame.setVisible(true);
   }
   
   public void setProperties(HashMap<String, PrologObject> properties)
   {
     super.setProperties(properties);
   }
   public Component getAWTComponent()
   {
      return frame;
   }

  public void insertChildBefore(ReactComponent child, ReactComponent sibling)
  {
     super.insertChildBefore(child, sibling);
     contentPane.insertChildBefore(child, sibling);
     repack();
  }

  public void removeChild(ReactComponent child)
  {
     super.removeChild(child);
     contentPane.removeChild(child);
     repack();
  }

  public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
  {
     super.replaceChild(newChild, oldChild);
     contentPane.replaceChild(newChild, oldChild);
     repack();
  }

   private void repack()
   {
      if (!userHasResizedFrame)
      {
         repackCount++;
         frame.pack();        
      }
   }
}
