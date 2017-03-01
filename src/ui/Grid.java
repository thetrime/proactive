package org.proactive.ui;

import javax.swing.JPanel;
import java.awt.Component;
import java.awt.Color;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.util.List;
import java.util.HashMap;
import java.util.Vector;
import org.proactive.ReactComponent;
import org.proactive.prolog.PrologObject;


public class Grid extends ReactComponent
{
   private JPanel component;
   private int columnCount = 0;
   private Vector<Integer> weights = new Vector<Integer>();
   private int x = 0;
   private int y = 0;
   private GridBagLayout layout;
   public Grid()
   {
      super();
      component = new JPanel();
      component.setBackground(new Color(150, 168, 200));
      layout = new GridBagLayout();
      component.setLayout(layout);
   }

   public Component getAWTComponent()
   {
      return component;
   }

   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
      boolean relayout = false;
      if (properties.containsKey("weights"))
      {
         weights = new Vector<Integer>();
         for (PrologObject p : properties.get("weights").asList())
         {
            weights.add(p.asInteger());
         }
         relayout = true;
      }
      if (properties.containsKey("columns"))
      {
         columnCount = properties.get("columns").asInteger();
         relayout = true;
      }

      if (relayout)
         relayout();
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      super.insertChildBefore(child, sibling);
      if (sibling == null)
         addChildToPanel(child);
      else
         relayout();
   }

   public void removeChild(ReactComponent child)
   {
      super.removeChild(child);
      relayout();
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      Component oldComponent = awtMap.get(oldChild);
      GridBagConstraints constraints = layout.getConstraints(oldComponent);
      component.remove(oldComponent);
      component.add(newChild.getAWTComponent(), constraints);
      super.replaceChild(newChild, oldChild);
   }


   private void relayout()
   {
      component.removeAll();
      x = 0;
      y = 0;
      for (ReactComponent c : children)
         addChildToPanel(c);
   }

   private void addChildToPanel(ReactComponent child)
   {
      System.out.println("Adding at " + x + ", " + y + " (of " + columnCount + ")");
      component.add(child.getAWTComponent(),
                    new GridBagConstraints(x, y, 1, 1, weights.elementAt(x), 0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0,0,0,0), 0, 0));
      if (x+1 == columnCount)
      {
         x = 0;
         y++;
      }
      else
         x++;

   }

}
