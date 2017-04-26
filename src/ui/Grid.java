package org.proactive.ui;

import javax.swing.JPanel;
import java.awt.Component;
import java.awt.Color;
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
   private int index = 0;
   private int padding = 0;
   private ProactiveGridLayout layout;
   public Grid()
   {
      super();
      component = new JPanel();
      component.setBackground(new Color(150, 168, 200));
      layout = new ProactiveGridLayout(weights, 0);
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
      boolean newLayout = false;
      if (properties.containsKey("weights"))
      {
         weights = new Vector<Integer>();
         for (PrologObject p : properties.get("weights").asList())
         {
            weights.add(p.asInteger());
         }
         columnCount = weights.size();
         newLayout = true;
         relayout = true;
      }
      if (properties.containsKey("padding"))
      {
         PrologObject t = properties.get("padding");
         if (t == null || t.isNull())
         {
            padding = 0;
         }
         else
         {
            String s = t.asString();
            if (s.endsWith("em"))
            {
               int em = (((javax.swing.JComponent)component).getFontMetrics(component.getFont()).charWidth('M'));
               padding = (int)(Float.parseFloat(s.substring(0, s.length()-2)) * em);
            }
            else if (s.endsWith("px"))
               padding = Integer.parseInt(s.substring(0, s.length()-2));
            else
               System.out.println("Bad padding: " + s);
         }
         newLayout = true;
      }
      if (newLayout)
      {
         layout = new ProactiveGridLayout(weights, padding);
         component.setLayout(layout);
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
      ProactiveConstraints constraints = layout.getConstraints(oldComponent);
      component.remove(oldComponent);
      component.add(newChild.getAWTComponent(), constraints);
      super.replaceChild(newChild, oldChild);
   }


   private void relayout()
   {
      component.removeAll();
      index = 0;
      for (ReactComponent c : children)
         addChildToPanel(c);
   }

   private void addChildToPanel(ReactComponent child)
   {
      component.add(child.getAWTComponent(),
                    new ProactiveConstraints(ProactiveConstraints.Fill.NONE,
                                             ProactiveConstraints.Alignment.AUTO,
                                             index++));
   }

}
