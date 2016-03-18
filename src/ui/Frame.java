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
   private Panel contentPane = new Panel("qqq");
   private boolean userHasResizedFrame = false;
   private int repackCount = 0;
   public Frame()
   {
      HashMap<String, PrologObject> contentPaneProperties = new HashMap<String, PrologObject>();
      contentPaneProperties.put("fill", new PrologObject(PrologObject.serializeObject("both")));
      contentPaneProperties.put("layout", new PrologObject(PrologObject.serializeObject("vertical")));
      contentPane.setProperties(contentPaneProperties);
      contentPane.setOwnerDocument(owner);
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

   public void setOwnerDocument(ReactWidget owner)
   {
      super.setOwnerDocument(owner);
      contentPane.setOwnerDocument(owner);
   }

   public void setParentNode(ReactComponent parent)
   {
      super.setParentNode(parent);
      if (parent == null)
         frame.dispose();
   }

   public void setProperties(HashMap<String, PrologObject> properties)
   {
     super.setProperties(properties);
     contentPane.setProperties(properties);
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
