package org.proactive.ui;

import java.awt.LayoutManager2;
import java.awt.Dimension;
import java.awt.Component;
import java.awt.Container;
import java.awt.Rectangle;
import java.awt.Insets;
import java.util.Map;
import java.util.HashMap;
import java.util.Vector;
import java.util.Comparator;
import java.util.Collections;

public class ProactiveLayoutManager implements LayoutManager2
{
   private Map<Component, ProactiveConstraints> map;
   public enum Layout {HORIZONTAL, VERTICAL};
   private enum Panic {TRUNCATE, CRUSH};
   public static Layout HORIZONTAL = Layout.HORIZONTAL;
   public static Layout VERTICAL = Layout.VERTICAL;
   private static final Panic panic = Panic.CRUSH;

   private ProactiveConstraints.Alignment alignment = ProactiveConstraints.Alignment.CENTER;
   private ProactiveConstraints.Justification justification = ProactiveConstraints.Justification.CENTER;
   private Layout layout = Layout.VERTICAL;
   public ProactiveLayoutManager(Layout layout, ProactiveConstraints.Alignment alignment, ProactiveConstraints.Justification justification)
   {
      map = new HashMap<Component, ProactiveConstraints>();
      this.layout = layout;
      this.alignment = alignment;
      this.justification = justification;
   }

   public void addLayoutComponent(String s, Component c) {} // Deliberately does nothing
   public void removeLayoutComponent(Component c) {}        // Deliberately does nothing
   
   public Dimension preferredLayoutSize(Container parent)
   {
      if (parent == null)
         return new Dimension (0, 0);
      int major = 0;
      int minor = 0;
      Insets parentInsets = parent.getInsets();
      for (Component c : parent.getComponents())
      {
         if (!c.isVisible()) continue;
         if (layout == Layout.HORIZONTAL)
         {
            major += c.getPreferredSize().getWidth();
            minor = (int)Math.max(c.getPreferredSize().getHeight(), minor);
         }
         else if (layout == Layout.VERTICAL)
         {
            major += c.getPreferredSize().getHeight();
            minor = (int)Math.max(c.getPreferredSize().getWidth(), minor);
         }
         else
            System.out.println("Illegal layout");
      }
      if (layout == Layout.HORIZONTAL)
         return new Dimension(major + parentInsets.left + parentInsets.right, minor + parentInsets.top + parentInsets.bottom);
      else if (layout == Layout.VERTICAL)
         return new Dimension(minor + parentInsets.left + parentInsets.right, major + parentInsets.top + parentInsets.bottom);
      else
         return new Dimension(0, 0);
   }

   public Dimension minimumLayoutSize(Container parent)
   {
      if (parent == null)
         return new Dimension (0, 0);
      int major = 0;
      int minor = 0;
      Insets parentInsets = parent.getInsets();
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
      //  System.out.println("Minimum Layout: (" + major + "," + minor + ")");
      if (layout == Layout.HORIZONTAL)
         return new Dimension(major + parentInsets.left + parentInsets.right, minor + parentInsets.top + parentInsets.bottom);
      else if (layout == Layout.VERTICAL)
         return new Dimension(minor + parentInsets.left + parentInsets.right, major + parentInsets.top + parentInsets.bottom);
      else
         return new Dimension(0, 0);
   }

   public Dimension maximumLayoutSize(Container parent)
   {
      return new Dimension (Integer.MAX_VALUE, Integer.MAX_VALUE);
   }

   public void layoutContainer(Container parent)
   {
      Component[] components = parent.getComponents();
      if (components.length == 0)
         return;
      Map<Component, Rectangle> proposedLayout = makeLayout(parent);
      for (Component c : components)
      {
         if (!c.isVisible())
            continue;
         Rectangle rect = proposedLayout.get(c);
         //System.out.println("--- Layout for component: " + c.getClass().getName() + " = " + rect);
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
      int minsum = 0;
      Map<Component, Rectangle> proposedLayout = new HashMap<Component, Rectangle>();
      Vector<Component> sortedComponents = new Vector<Component>();

      for (Component c : parent.getComponents())
      {
         if (!c.isVisible()) continue;
         sortedComponents.add(c);
         if (layout == Layout.HORIZONTAL)
         {
            sum += c.getPreferredSize().getWidth();
            minsum += c.getMinimumSize().getWidth();
         }
         else if (layout == Layout.VERTICAL)
         {
            sum += c.getPreferredSize().getHeight();
            minsum += c.getMinimumSize().getHeight();
         }
      }
      Collections.sort(sortedComponents, new Comparator<Component>()
                       {
                       @Override
                       public int compare(Component o1, Component o2)
                       {
                       return map.get(o1).index - map.get(o2).index;
                       }
      });
      int major_available = 0;
      int minor_available = 0;
      int major_pad = 0;
      int minor_pad = 0;
      Insets parentInsets = parent.getInsets();

      if (layout == Layout.HORIZONTAL)
      {
         major_available = (int)parent.getBounds().getWidth() - parentInsets.left - parentInsets.right;
         major_pad = parentInsets.left;
         minor_available = (int)parent.getBounds().getHeight() - parentInsets.top - parentInsets.bottom;
         minor_pad = parentInsets.top;
      }
      else if (layout == Layout.VERTICAL)
      {
         major_available = (int)parent.getBounds().getHeight() - parentInsets.top - parentInsets.bottom;
         major_pad = parentInsets.top;
         minor_available = (int)parent.getBounds().getWidth() - parentInsets.left - parentInsets.right;
         minor_pad = parentInsets.left;
      }
      //System.out.println("    We have " + major_available + " x " + minor_available +" to work with");
      //System.out.println("Layout: " + layout);
      if (sum <= major_available)
      {
         double extraSpace = major_available - sum;
         // We have enough space to display all the components at their preferred size. In this case if a component has:
         //   Fill NONE: Make it the preferred size
         //   Fill == Layout or Fill == BOTH: If there are N of these components, make them preferred size + (extraSpace/N)
         //   Otherwise: Make them preferred size

         int fillCount = 0;
         int componentCount = 0;
         for (Component c : parent.getComponents())
         {
            if (!c.isVisible()) continue;
            componentCount++;
            ProactiveConstraints.Fill f = map.get(c).fill;
            if (f == ProactiveConstraints.Fill.BOTH ||
                (f == ProactiveConstraints.Fill.HORIZONTAL && layout == Layout.HORIZONTAL) ||
                (f == ProactiveConstraints.Fill.VERTICAL && layout == Layout.VERTICAL))
               fillCount++;
         }
         //System.out.println("We have " + fillCount + " components which require fill in our " + major_available + " sized component");
         // Note that if fillCount is 0 we must use the justification
         int intraPad = 0;
         int beforePad = 0;
         if (fillCount == 0)
         {
            if (justification == ProactiveConstraints.Justification.START)
            {
               beforePad = 0;
               intraPad = 0;
            }
            else if (justification == ProactiveConstraints.Justification.END)
            {
               beforePad = major_available - sum;
               intraPad = 0;
            }
            else if (justification == ProactiveConstraints.Justification.CENTER)
            {
               beforePad = (int)((major_available - sum)/2);
               intraPad = 0;
            }
            else if (justification == ProactiveConstraints.Justification.SPACE_BETWEEN)
            {
               beforePad = 0;
               if (componentCount > 1)
                  intraPad = (int)((major_available - sum)/(componentCount-1));
               else
                  intraPad = 0;
            }
            else if (justification == ProactiveConstraints.Justification.SPACE_AROUND)
            {
               intraPad = (int)((major_available - sum)/(componentCount));
               beforePad = (int)((major_available - sum)/(2*componentCount));
            }
         }

         // Now we can set the placing on all the components
         int major_position = beforePad + major_pad;
         for (Component c : sortedComponents)
         {
            if (!c.isVisible()) continue;
            ProactiveConstraints constraints = map.get(c);

            int major_scale = 0;
            int minor_scale = 0;
            int minor_position = minor_pad;
            if (layout == Layout.HORIZONTAL)
            {
               major_scale = (int)c.getPreferredSize().getWidth();
               if (constraints.fill == ProactiveConstraints.Fill.VERTICAL || constraints.fill == ProactiveConstraints.Fill.BOTH)
                  minor_scale = minor_available;
               else if (constraints.fill == ProactiveConstraints.Fill.HORIZONTAL)
                  minor_scale = minor_available;
               else
                  minor_scale = (int)Math.min(c.getPreferredSize().getHeight(), minor_available);
            }
            else if (layout == Layout.VERTICAL)
            {
               major_scale = (int)c.getPreferredSize().getHeight();
               if (constraints.fill == ProactiveConstraints.Fill.HORIZONTAL || constraints.fill == ProactiveConstraints.Fill.BOTH)
                  minor_scale = minor_available;
               else if (constraints.fill == ProactiveConstraints.Fill.VERTICAL)
                  minor_scale = minor_available;
               else
                  minor_scale = (int)Math.min(c.getPreferredSize().getWidth(), minor_available);
            }

            minor_position = minor_pad + getAlignmentOffset(constraints, minor_available, minor_scale);

            if (constraints.fill == ProactiveConstraints.Fill.BOTH ||
                (constraints.fill == ProactiveConstraints.Fill.HORIZONTAL && layout == Layout.HORIZONTAL) ||
                (constraints.fill == ProactiveConstraints.Fill.VERTICAL && layout == Layout.VERTICAL))
            {
               proposedLayout.put(c, new Rectangle(major_position, minor_position, major_scale + (int)(extraSpace / (double)fillCount), minor_scale));
               major_position += (major_scale + (int)(extraSpace / (double)fillCount)) + intraPad;
            }
            else
            {
               proposedLayout.put(c, new Rectangle(major_position, minor_position, major_scale, minor_scale));
               major_position += major_scale + intraPad;
            }
         }
      }
      else
      {
         //System.out.println("    -> Not enough space: " + sum + " < " + major_available);
         // We do not have enough space to display everything at its requested size. This is where we would potentially reflow components
         // but for now, just display everything in proportion to its preferred major size
         // One more final consideration: TRY and not make any component smaller than its minimum size in the major direction
         // Note that if there is not enough space to display everything at its minimum size, then we have two options:
         //   1) Truncate the items (give them 0 major scale once we are out of space)
         //   2) Crush items so they all fit
         // Which we do is governed by the static final field 'panic'
         int major_position = major_pad;
         int minor_position = minor_pad;
         for (Component c : sortedComponents)
         {
            if (!c.isVisible()) continue;
            ProactiveConstraints constraints = map.get(c);
            int major_scale = 0;
            int minor_scale = 0;
            int major_minimum = 0;
            if (layout == Layout.HORIZONTAL)
            {
               major_minimum = (int)c.getMinimumSize().getWidth();
               major_scale = (int)((c.getPreferredSize().getWidth() / (double)sum) * (double)major_available);
               if (major_scale < major_minimum && panic == Panic.TRUNCATE)
               {
                  // We have stolen some space. Reduce the major_available to reflect that
                  major_available -= (major_minimum - major_scale);
                  if (major_available < 0)
                     major_available = 0;
                  major_scale = major_minimum;
               }
               if (constraints.fill == ProactiveConstraints.Fill.VERTICAL || constraints.fill == ProactiveConstraints.Fill.BOTH)
                  minor_scale = minor_available;
               else if (constraints.fill == ProactiveConstraints.Fill.HORIZONTAL)
                  minor_scale = minor_available;
               else
                  minor_scale = (int)Math.min(c.getPreferredSize().getHeight(), minor_available);
            }
            else if (layout == Layout.VERTICAL)
            {
               major_minimum = (int)c.getMinimumSize().getHeight();
               major_scale = (int)((c.getPreferredSize().getHeight() / (double)sum) * (double)major_available);
               if (major_scale < major_minimum && panic == Panic.TRUNCATE)
               {
                  // We have stolen some space. Reduce the major_available to reflect that
                  major_available -= (major_minimum - major_scale);
                  if (major_available < 0)
                     major_available = 0; // Disappointing :(
                  major_scale = major_minimum;
               }
               if (constraints.fill == ProactiveConstraints.Fill.HORIZONTAL || constraints.fill == ProactiveConstraints.Fill.BOTH)
                  minor_scale = minor_available;
               else if (constraints.fill == ProactiveConstraints.Fill.VERTICAL)
                  minor_scale = minor_available;
               else
                  minor_scale = (int)Math.min(c.getPreferredSize().getWidth(), minor_available);
            }
            minor_position = minor_pad + getAlignmentOffset(constraints, minor_available, minor_scale);

            proposedLayout.put(c, new Rectangle(major_position, minor_position, major_scale, minor_scale));
            major_position += major_scale;
         }
      }
      return proposedLayout;
   }

   private int getAlignmentOffset(ProactiveConstraints constraints, int minor_available, int minor_scale)
   {
      ProactiveConstraints.Alignment itemAlignment = constraints.selfAlignment;
      if (itemAlignment == ProactiveConstraints.Alignment.AUTO)
         itemAlignment = alignment;
      // This is a bit of a quirk: .no_fill { align-self: center; }
      if (constraints.fill == ProactiveConstraints.Fill.NONE)
         itemAlignment = ProactiveConstraints.Alignment.CENTER;
      if (itemAlignment == ProactiveConstraints.Alignment.START)
         return 0;
      else if (itemAlignment == ProactiveConstraints.Alignment.END)
         return minor_available - minor_scale;
      else if (itemAlignment == ProactiveConstraints.Alignment.CENTER)
         return (int)((minor_available - minor_scale)/2);
      else if (itemAlignment == ProactiveConstraints.Alignment.BASELINE)
         return 0; // This is not actually supported!
      else if (itemAlignment == ProactiveConstraints.Alignment.STRETCH)
         return 0; //  FIXME: minor_scale should also be set to minor_available?
      // Should not be reachable
      return 0;
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
