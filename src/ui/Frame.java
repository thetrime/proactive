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
import org.proactive.ReactWidget;

public class Frame extends ReactComponent 
{
   protected JFrame frame = new JFrame();
   private boolean userHasResizedFrame = false;
   private ReactComponent contentPane;
   private int repackCount = 0;
   public Frame()
   {
      frame.getContentPane().setLayout(new BorderLayout());
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

   public void setParentNode(ReactComponent parent)
   {
      super.setParentNode(parent);
      if (parent == null)
         frame.dispose();
   }

   public Component getAWTComponent()
   {
      return frame;
   }

  public void insertChildBefore(ReactComponent child, ReactComponent sibling)
  {
     super.insertChildBefore(child, sibling);
     this.contentPane = child;
     frame.getContentPane().removeAll();
     frame.getContentPane().add(contentPane.getAWTComponent(), BorderLayout.CENTER);
     repack();
  }

  public void removeChild(ReactComponent child)
  {
     super.removeChild(child);
     contentPane = null;
     frame.getContentPane().removeAll();
     repack();
  }

  public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
  {
     super.replaceChild(newChild, oldChild);
     frame.getContentPane().removeAll();
     contentPane = newChild;
     frame.getContentPane().add(contentPane.getAWTComponent(), BorderLayout.CENTER);
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
