package org.proactive.ui;

import java.awt.LayoutManager2;
import java.awt.Dimension;
import java.awt.Component;
import java.awt.Container;
import java.awt.Rectangle;
import java.util.Map;
import java.util.HashMap;

public class ProactiveLayoutManager implements LayoutManager2
{
   private Map<Component, ProactiveConstraints> map;
   public enum Layout {HORIZONTAL, VERTICAL};
   public static Layout HORIZONTAL = Layout.HORIZONTAL;
   public static Layout VERTICAL = Layout.VERTICAL;
   private Layout layout = Layout.VERTICAL;
   private ProactiveConstraints.Fill fill;
   public ProactiveLayoutManager(Layout layout)
   {
      map = new HashMap<Component, ProactiveConstraints>();
      System.out.println("Set layout to: " + layout);
      this.layout = layout;
   }

   public void addLayoutComponent(String s, Component c) {} // Deliberately does nothing
   public void removeLayoutComponent(Component c) {}        // Deliberately does nothing
   
   public Dimension preferredLayoutSize(Container parent)
   {
      if (parent == null)
         return new Dimension (0, 0);
      int major = 0;
      int minor = 0;
      System.out.println("Computing preferred layout for " + parent.getComponents().length + " components in " + parent);
      for (Component c : parent.getComponents())
      {
         if (!c.isVisible()) continue;
         if (layout == Layout.HORIZONTAL)
         {
            System.out.println("Horizontal");
            major += c.getPreferredSize().getWidth();
            minor = (int)Math.max(c.getPreferredSize().getHeight(), minor);
         }
         else if (layout == Layout.VERTICAL)
         {
            System.out.println("Vertical");
            major += c.getPreferredSize().getHeight();
            minor = (int)Math.max(c.getPreferredSize().getWidth(), minor);
         }
         else
            System.out.println("Illegal layout");
      }
      System.out.println("PreferredLayout: (" + major + "," + minor + ")");
      if (layout == Layout.HORIZONTAL)
         return new Dimension(major, minor);
      else if (layout == Layout.VERTICAL)
         return new Dimension(minor, major);
      else
         return new Dimension(0, 0);
   }

   public Dimension minimumLayoutSize(Container parent)
   {
      if (parent == null)
         return new Dimension (0, 0);
      int major = 0;
      int minor = 0;
      for (Component c : parent.getComponents())
      {
         if (!c.isVisible()) continue;
         if (layout == Layout.HORIZONTAL)
         {
            major += c.getMinimumSize().getWidth();
            minor = (int)Math.max(c.getMinimumSize().getHeight(), minor);
         }
         else if (layout == Layout.VERTICAL)
         {
            major += c.getMinimumSize().getHeight();
            minor = (int)Math.max(c.getMinimumSize().getWidth(), minor);
         }
      }
      System.out.println("Minimum Layout: (" + major + "," + minor + ")");
      if (layout == Layout.HORIZONTAL)
         return new Dimension(major, minor);
      else if (layout == Layout.VERTICAL)
         return new Dimension(minor, major);
      else
         return new Dimension(0, 0);
   }

   public Dimension maximumLayoutSize(Container parent)
   {
      return new Dimension (Integer.MAX_VALUE, Integer.MAX_VALUE);
   }

   public void layoutContainer(Container parent)
   {
      System.out.println("Laying out...");
      Component[] components = parent.getComponents();
      if (components.length == 0)
         return;
      Map<Component, Rectangle> proposedLayout = makeLayout(parent);
      for (Component c : components)
      {
         if (!c.isVisible())
            continue;
         Rectangle rect = proposedLayout.get(c);
         System.out.println("Layout for component: " + rect);
         if (layout == Layout.HORIZONTAL)
            c.setBounds((int)rect.getX(), (int)rect.getY(), (int)rect.getWidth(), (int)rect.getHeight());
         else if (layout == Layout.VERTICAL)
            c.setBounds((int)rect.getY(), (int)rect.getX(), (int)rect.getHeight(), (int)rect.getWidth());
      }
   }

   Map<Component, Rectangle> makeLayout(Container parent)
   {
      // First see what the total preferred major size is
      int sum = 0;
      Map<Component, Rectangle> proposedLayout = new HashMap<Component, Rectangle>();
      for (Component c : parent.getComponents())
      {
         if (!c.isVisible()) continue;
         if (layout == Layout.HORIZONTAL)
            sum += c.getPreferredSize().getWidth();
         else if (layout == Layout.VERTICAL)
            sum += c.getPreferredSize().getHeight();
      }
      int major_available = 0;
      int minor_available = 0;
      if (layout == Layout.HORIZONTAL)
      {
         major_available = (int)parent.getSize().getWidth();
         minor_available = (int)parent.getSize().getHeight();
      }
      else if (layout == Layout.VERTICAL)
      {
         major_available = (int)parent.getSize().getHeight();
         minor_available = (int)parent.getSize().getWidth();
      }

      if (sum < major_available)
      {
         double extraSpace = major_available - sum;
         // We have enough space to display all the components at their preferred size. In this case if a component has:
         //   Fill NONE: Make it the preferred size
         //   Fill == Layout or Fill == BOTH: If there are N of these components, make them preferred size + (extraSpace/N)
         //   Otherwise: Make them preferred size
         int fillCount = 0;
         for (Component c : parent.getComponents())
         {
            if (!c.isVisible()) continue;
            ProactiveConstraints.Fill f = map.get(c).fill;
            if (f == ProactiveConstraints.Fill.BOTH ||
                (f == ProactiveConstraints.Fill.HORIZONTAL && layout == Layout.HORIZONTAL) ||
                (f == ProactiveConstraints.Fill.VERTICAL && layout == Layout.VERTICAL))
               fillCount++;
         }
         // Now we can set the placing on all the components
         int major_position = 0;
         int minor_position = 0;
         for (Component c : parent.getComponents())
         {
            if (!c.isVisible()) continue;
            ProactiveConstraints.Fill f = map.get(c).fill;
            int major_scale = 0;
            int minor_scale = 0;
            if (layout == Layout.HORIZONTAL)
            {
               major_scale = (int)c.getPreferredSize().getWidth();
               minor_scale = (int)Math.min(c.getPreferredSize().getHeight(), minor_available);
            }
            else if (layout == Layout.VERTICAL)
            {
               major_scale = (int)c.getPreferredSize().getHeight();
               minor_scale = (int)Math.min(c.getPreferredSize().getWidth(), minor_available);
            }
            if (f == ProactiveConstraints.Fill.BOTH ||
                (f == ProactiveConstraints.Fill.HORIZONTAL && layout == Layout.HORIZONTAL) ||
                (f == ProactiveConstraints.Fill.VERTICAL && layout == Layout.VERTICAL))
            {
               proposedLayout.put(c, new Rectangle(major_position, 0, major_scale + (int)(extraSpace / (double)fillCount), minor_scale));
               major_position += (major_scale + (int)(extraSpace / (double)fillCount));
            }
            else
            {
               proposedLayout.put(c, new Rectangle(major_position, 0, major_scale, minor_scale));
               major_position += major_scale;
            }
         }
      }
      else
      {
         // We do not have enough space to display everything at its requested size. This is where we would potentially reflow components
         // but for now, just display everything in proportion to its preferred major size
         int major_position = 0;
         int minor_position = 0;
         for (Component c : parent.getComponents())
         {
            if (!c.isVisible()) continue;
            int major_scale = 0;
            int minor_scale = 0;
            if (layout == Layout.HORIZONTAL)
            {
               major_scale = (int)((c.getPreferredSize().getWidth() / (double)sum) * (double)major_available);
               minor_scale = (int)Math.min(c.getPreferredSize().getHeight(), minor_available);
            }
            else if (layout == Layout.VERTICAL)
            {
               major_scale = (int)((c.getPreferredSize().getHeight() / (double)sum) * (double)major_available);
               minor_scale = (int)Math.min(c.getPreferredSize().getWidth(), minor_available);
            }
            proposedLayout.put(c, new Rectangle(major_position, 0, major_scale, minor_scale));
            major_position += major_scale;
         }
      }
      return proposedLayout;
   }

   public void invalidateLayout(Container target)
   {
      // FIXME: Implement?
   }

   public void addLayoutComponent(Component c, Object constraints)
   {
      if (constraints == null)
         return;
      if (!(constraints instanceof ProactiveConstraints))
         throw new IllegalArgumentException("Constraints must be ProactiveConstraints. Received: " + constraints.getClass().getName());
      setConstraints(c, (ProactiveConstraints)constraints);
   }

   public void setConstraints(Component c, ProactiveConstraints newConstraints)
   {
      map.put(c, newConstraints);
   }

   public ProactiveConstraints getConstraints(Component c)
   {
      return map.get(c);
   }

   public float getLayoutAlignmentX(Container c)
   {
      return Component.CENTER_ALIGNMENT;
   }
   public float getLayoutAlignmentY(Container c)
   {
      return Component.CENTER_ALIGNMENT;
   }

}
