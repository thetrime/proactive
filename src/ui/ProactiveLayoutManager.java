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
import javax.swing.JScrollPane;
import java.awt.LayoutManager;

public class ProactiveLayoutManager implements LayoutManager2
{
   private Map<Component, ProactiveConstraints> map;
   public enum Layout {HORIZONTAL, VERTICAL};
   public enum Wrap {NOWRAP, WRAP, WRAP_REVERSE};
   private enum Panic {TRUNCATE, CRUSH};
   public static Layout HORIZONTAL = Layout.HORIZONTAL;
   public static Layout VERTICAL = Layout.VERTICAL;
   private static final Panic panic = Panic.CRUSH;

   private ProactiveConstraints.Alignment alignment = ProactiveConstraints.Alignment.CENTER;
   private ProactiveConstraints.Justification justification = ProactiveConstraints.Justification.CENTER;
   private Layout layout = Layout.VERTICAL;
   private Wrap wrap = Wrap.NOWRAP;
   public ProactiveLayoutManager(LayoutManager oldManager, Layout layout, ProactiveConstraints.Alignment alignment, ProactiveConstraints.Justification justification, Wrap wrap)
   {
      if (oldManager != null && oldManager instanceof ProactiveLayoutManager)
         map = ((ProactiveLayoutManager)oldManager).map;
      else
         map = new HashMap<Component, ProactiveConstraints>();
      this.layout = layout;
      this.wrap = wrap;
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

   private static boolean isCrushable(Container c)
   {
      for (Component t: c.getComponents())
      {
         if (t instanceof JScrollPane)
            return true;
         if (t instanceof Container && isCrushable((Container)t))
            return true;
      }
      return false;
   }

   Map<Component, Rectangle> makeLayout(Container parent)
   {
      // First see what the total preferred major size is
      int sum = 0;
      int minsum = 0;
      Map<Component, Rectangle> proposedLayout = new HashMap<Component, Rectangle>();
      Vector<Component> sortedComponents = new Vector<Component>();
      Vector<Component> crushables = new Vector<Component>();

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
            //System.out.println(c + " -> " + c.getMinimumSize().getHeight());
            minsum += c.getMinimumSize().getHeight();
         }
         if (c instanceof JScrollPane)
            crushables.add(c);
         else if (c instanceof Container && isCrushable((Container)c))
            crushables.add(c);
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
      //System.out.println("    Laying out a " + layout + " panel with " + sortedComponents.size() + " components on it");
      //System.out.println("    We have " + major_available + " (major), " + minor_available +" (minor) to work with");
      //System.out.println("    We need " + sum + " in the major direction for full layout, and at least " + minsum + " to lay out the " + sortedComponents.size() + " components");

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

            // Compute how much extra space this component will be granted - the 'slush'
            int slush = 0;
            if (constraints.fill == ProactiveConstraints.Fill.BOTH ||
                (constraints.fill == ProactiveConstraints.Fill.HORIZONTAL && layout == Layout.HORIZONTAL) ||
                (constraints.fill == ProactiveConstraints.Fill.VERTICAL && layout == Layout.VERTICAL))
               slush =  (int)(extraSpace / (double)fillCount);

            if (layout == Layout.HORIZONTAL)
            {
               major_scale = (int)c.getPreferredSize().getWidth();
               if (c.getMaximumSize() != null && c.getMaximumSize().getWidth() < major_scale + slush)
               {
                  major_position += (major_scale + slush - c.getMaximumSize().getWidth());
                  major_scale = (int)c.getMaximumSize().getWidth();
                  slush = 0;
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
               major_scale = (int)c.getPreferredSize().getHeight();
               if (constraints.fill == ProactiveConstraints.Fill.HORIZONTAL || constraints.fill == ProactiveConstraints.Fill.BOTH)
                  minor_scale = minor_available;
               else if (constraints.fill == ProactiveConstraints.Fill.VERTICAL)
                  minor_scale = minor_available;
               else
                  minor_scale = (int)Math.min(c.getPreferredSize().getWidth(), minor_available);
            }
            minor_position = minor_pad + getAlignmentOffset(constraints, minor_available, minor_scale);
            proposedLayout.put(c, new Rectangle(major_position, minor_position, major_scale + slush, minor_scale));
            //System.out.println("   -> Placing child at major=" + major_position + ", minor=" + minor_position + ", with major_width=" + (major_scale + slush) + " and minor_width=" + minor_scale);
            major_position += major_scale + slush + intraPad;
         }
      }
      else if (crushables.size() == 0 && wrap != Wrap.NOWRAP)
      {
         // Not enough space and no handy scrollpanes. But we have been asked to wrap instead. So, display everything at its preferred size (if possible), but bump
         // down (or across) to the next row (or column) as needed. If we do not have enough major space even for a single unit, then crush it
         // If we run out of minor space, truncate.
         // FIXME: We need to adjust preferredLayoutSize and minimumLayoutSize above as well

         int major_position = major_pad;
         int minor_position = minor_pad;
         int major_remaining = major_available;
         int components_so_far_this_block = 0;
         int slush = 0;
         int intraPad = 0;
         for (Component c : sortedComponents)
         {
            if (!c.isVisible()) continue;
            ProactiveConstraints constraints = map.get(c);
            int major_scale = 0;
            int minor_scale = 0;
            major_remaining = major_available - major_position;
            if (layout == Layout.HORIZONTAL)
            {
               if (major_remaining < c.getMinimumSize().getWidth() && components_so_far_this_block > 0)
               {
                  // Bump to the next row
                  major_position = major_pad;
                  minor_position += c.getPreferredSize().getHeight();
                  components_so_far_this_block = 0;
               }
               major_scale = (int)c.getPreferredSize().getWidth();
               if (major_scale > major_available)
               {
                  // Otherwise, we have to make do
                  major_scale = major_available;
               }
               if (c.getMaximumSize() != null && c.getMaximumSize().getWidth() < major_scale)
               {
                  major_position += (major_scale - c.getMaximumSize().getWidth());
                  major_scale = (int)c.getMaximumSize().getWidth();
               }
               minor_scale = (int)c.getPreferredSize().getHeight();
            }
            else if (layout == Layout.VERTICAL)
            {
               if (major_remaining < c.getMinimumSize().getHeight() && components_so_far_this_block > 0)
               {
                  // Bump to the next row
                  major_position = major_pad;
                  minor_position += c.getPreferredSize().getWidth();
                  components_so_far_this_block = 0;
               }
               major_scale = (int)c.getPreferredSize().getHeight();
               if (major_scale > major_available)
               {
                  // Otherwise, we have to make do
                  major_scale = major_available;
               }
               if (c.getMaximumSize() != null && c.getMaximumSize().getHeight() < major_scale)
               {
                  major_position += (major_scale - c.getMaximumSize().getHeight());
                  major_scale = (int)c.getMaximumSize().getHeight();
               }
               minor_scale = (int)c.getPreferredSize().getWidth();
            }
            proposedLayout.put(c, new Rectangle(major_position, minor_position, major_scale + slush, minor_scale));
            //System.out.println("   -> Placing child at major=" + major_position + ", minor=" + minor_position + ", with major_width=" + (major_scale + slush) + " and minor_width=" + minor_scale);
            major_position += major_scale + slush + intraPad;
            components_so_far_this_block++;
         }
      }
      else // Not enough room for everything at its preferred size. However, there is one more important thing to consider: scrollpanes.
      {

         // This is where we need to think about scrollpanes. If we have more space than is needed to display everything at the minimum size then
         // we need to treat scrollpanes differently; not just proportional to the preferred major size. If we keep track of the minimum size of
         // all the components that arent scrollpanes, we can find out how much is left over, divide that by the number of scrollpanes, and
         // make each scrollpane that big. If there are NO scrollpanes, then we must proceed as before
         // Note that this applies to scrollpanes nested deep in children, too, but they should return their minimum size as 0x0, which means they
         // wont count toward the minimum size of the panels that they're embedded in anyway.
         int crushable_space = 0;
         if (crushables.size() > 0 && minsum <= major_available)
         {
            // Ok, we can just display everything at minimum size and leave the remaining space to the crushables (ie scrollpanes)
            crushable_space = (int)((major_available - minsum) / crushables.size());
            //System.out.println("   Crushable space: " + crushable_space);
            //System.out.println("   Major space: " + major_available);
            //System.out.println("   Minsum: " + minsum);
         }


         // We do not have enough space to display everything at its requested size. This
         // but for now, just display everything in proportion to its preferred major size
         // One more final consideration: TRY and not make any component smaller than its minimum size in the major direction
         // Note that if there is not enough space to display everything at its minimum size, then we have two options:
         //   1) Truncate the items (give them 0 major scale once we are out of space)
         //   2) Crush items so they all fit
         // Which we do is governed by the static final field 'panic'
         int major_position = major_pad;
         int minor_position = minor_pad;
         //System.out.println("   Laying out " + sortedComponents.size() + " components in " + major_available + " space");
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
               if (crushables.contains(c))
                  major_scale = crushable_space + major_minimum;
               else if (crushables.size() > 0)
                  major_scale = (int)(c.getMinimumSize().getWidth());
               else
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
               if (crushables.contains(c))
                  major_scale = crushable_space + major_minimum;
               else if (crushables.size() > 0)
                  major_scale = (int)(c.getMinimumSize().getHeight());
               else
                  major_scale = (int)((c.getPreferredSize().getHeight() / (double)sum) * (double)major_available);
               //System.out.println("      Setting major scale to " + major_scale);
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
            //System.out.println("      -> Setting at " + major_position + "," + minor_position + " with size " + major_scale + "x" + minor_scale);
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
