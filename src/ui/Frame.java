package org.proactive.ui;

import javax.swing.JFrame;
import javax.swing.KeyStroke;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.AbstractAction;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.KeyEvent;
import java.awt.event.ActionEvent;
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
      frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
      frame.setVisible(true);
   }

   private WindowListener windowListener = null;
   private void setCloseHandler(PrologObject value)
   {
      KeyStroke keyStrokeEscape = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, false);
      if (windowListener != null)
         frame.removeWindowListener(windowListener);
      if (value.isNull())
      {
         ((InputMap)frame.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)).put(keyStrokeEscape, "none");
         return;
      }
      windowListener = new WindowAdapter()
	 {
	    public void windowClosing(WindowEvent ev)
	    {
	       try
               {
		  getOwnerDocument().triggerEvent(value.asTerm(), PrologObject.emptyList().asTerm());
               }
               catch (Exception e)
               {
                  e.printStackTrace();
               }
            }
         };
      frame.addWindowListener(windowListener);
      ((InputMap)frame.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)).put(keyStrokeEscape, "cancel-action");
      frame.getRootPane().getActionMap().put("cancel-action", new AbstractAction("cancel-action")
         {
            public void actionPerformed(ActionEvent event)
            {
               try
               {
                  getOwnerDocument().triggerEvent(value.asTerm(), PrologObject.emptyList().asTerm());
               }
               catch (Exception e)
               {
                  e.printStackTrace();
               }
            }
         });
   }

   @Override
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
      if (properties.containsKey("onClose"))
	 setCloseHandler(properties.get("onClose"));
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
