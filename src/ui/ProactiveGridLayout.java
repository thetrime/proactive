package org.proactive.ui;

import java.awt.LayoutManager2;
import java.awt.Dimension;
import java.awt.Container;
import java.awt.Component;
import java.awt.Insets;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;
import java.util.Comparator;
import java.util.Arrays;

public class ProactiveGridLayout implements LayoutManager2
{
   private int[] weights;
   private int total_weight;
   private Map<Component, ProactiveConstraints> map;
   private int[] preferred_column_widths;
   private int[] minimum_column_widths;
   private Vector<Integer> row_heights;

   public ProactiveGridLayout(Vector<Integer> weights)
   {
      map = new HashMap<Component, ProactiveConstraints>();
      this.weights = new int[weights.size()];
      preferred_column_widths = new int[weights.size()];
      minimum_column_widths = new int[weights.size()];
      row_heights = new Vector<Integer>();
      int j = 0;
      for(int i: weights)
      {
         this.weights[j] = i;
         total_weight += i;
         preferred_column_widths[j] = 0;
         minimum_column_widths[j] = 0;
         j++;
      }
   }
   public void addLayoutComponent(String s, Component c) {} // Deliberately does nothing
   public void removeLayoutComponent(Component c) {}        // Deliberately does nothing
   public Dimension preferredLayoutSize(Container parent)
   {
      if (parent == null)
         return new Dimension (0, 0);
      preferred_column_widths = new int[weights.length];
      row_heights = new Vector<Integer>();
      Insets parentInsets = parent.getInsets();
      // This is the preferred width of the widest row by the sum of the preferred height of the maximum element in each row
      int height = 0;
      int width = 0;
      Component[] sortedComponents = parent.getComponents();
      Arrays.sort(sortedComponents, new Comparator<Component>()
                       {
                          @Override
                          public int compare(Component o1, Component o2)
                          {
                             return map.get(o1).index - map.get(o2).index;
                          }
         });
      int componentCount = sortedComponents.length;
      int counted = 0;
      for (int row = 0; counted < componentCount; row++)
      {
         int row_height = 0;
         int row_width = 0;
         for (int col = 0; col < weights.length; col++)
         {
            int item_width = (int)sortedComponents[counted].getPreferredSize().getWidth();
            row_width += item_width;
            if (preferred_column_widths[col] < item_width)
               preferred_column_widths[col] = item_width;
            if (sortedComponents[counted].getPreferredSize().getHeight() > row_height)
               row_height = (int)sortedComponents[counted].getPreferredSize().getHeight();
            counted++;
         }
         if (row_width > width)
            width = row_width;
         row_heights.add(row_height);
         height += row_height;
      }
      return new Dimension(width + parentInsets.left + parentInsets.right, height + parentInsets.top + parentInsets.bottom);
   }
   public Dimension minimumLayoutSize(Container parent)
   {
      if (parent == null)
         return new Dimension (0, 0);
      // This is the minimum width of the widest row by the sum of the minimum height of the maximum element in each row
      int height = 0;
      int width = 0;
      Insets parentInsets = parent.getInsets();
      Component[] sortedComponents = parent.getComponents();
      Arrays.sort(sortedComponents, new Comparator<Component>()
                       {
                          @Override
                          public int compare(Component o1, Component o2)
                          {
                             return map.get(o1).index - map.get(o2).index;
                          }
         });
      int componentCount = sortedComponents.length;
      int counted = 0;
      for (int row = 0; counted < componentCount; row++)
      {
         int row_height = 0;
         int row_width = 0;
         for (int col = 0; col < weights.length; col++)
         {
            int item_width = (int)sortedComponents[counted].getMinimumSize().getWidth();
            row_width += item_width;
            if (minimum_column_widths[col] < item_width)
               minimum_column_widths[col] = item_width;
            if (sortedComponents[counted].getMinimumSize().getHeight() > row_height)
               row_height = (int)sortedComponents[counted].getMinimumSize().getHeight();
            counted++;
         }
         if (row_width > width)
            width = row_width;
         height += row_height;
      }
      return new Dimension(width + parentInsets.left + parentInsets.right, height + parentInsets.top + parentInsets.bottom);
   }

   public Dimension maximumLayoutSize(Container parent)
   {
      return new Dimension (Integer.MAX_VALUE, Integer.MAX_VALUE);
   }

   public void layoutContainer(Container parent)
   {
      Component[] sortedComponents = parent.getComponents();
      Arrays.sort(sortedComponents, new Comparator<Component>()
                       {
                          @Override
                          public int compare(Component o1, Component o2)
                          {
                             return map.get(o1).index - map.get(o2).index;
                          }
         });
      Insets parentInsets = parent.getInsets();
      int available_height = (int)parent.getBounds().getHeight() - parentInsets.top - parentInsets.bottom;
      int available_width = (int)parent.getBounds().getWidth() - parentInsets.left - parentInsets.right;
      int componentCount = sortedComponents.length;
      int counted = 0;

      // These calls will also set up preferred_column_widths and minimum_column_widths
      Dimension preferredSize = preferredLayoutSize(parent);
      Dimension minimumSize = minimumLayoutSize(parent);

      // Now we must compute the actual column widths
      int[] column_widths = new int[preferred_column_widths.length];

      if (available_width >= preferredSize.getWidth())
      {
         //System.out.println("Happy width case: " + available_width + " vs " + preferredSize.getWidth());
         // Happy case - there is enough space, plus we can expand some components out according to their weight
         // If everything has weight 0 and there is space left over, then just ignore it
         double extra_width = available_width - preferredSize.getWidth();
         for (int i = 0; i < column_widths.length; i++)
         {
            if (weights[i] == 0)
               column_widths[i] = preferred_column_widths[i];
            else
               column_widths[i] = (int)(preferred_column_widths[i] + (((double)(weights[i])/(double)total_weight) * extra_width));
         }
      }
      else if (available_width >= minimumSize.getWidth())
      {
         //System.out.println("Tolerable width case");
         // Tolerable case - we can display things at the minimum size and still fit everything without truncation
         // To avoid the jarring 'collapsing' behaviour of GridBagLayout, we should distribute any leftover space
         // (after setting each one to the minimum size) between all the components evenly, ignoring the weights
         double extra_width = available_width - minimumSize.getWidth();
         for (int i = 0; i < column_widths.length; i++)
         {
            column_widths[i] = (int)(minimum_column_widths[i] + ((double)extra_width)/((double)weights.length));
         }
      }
      else
      {
         // Sad case - even at minimum size we cannot fit everything in. Display components at either
         // minimum-size * crush-factor or display them at minimum-size until we cannot fit any more in, then truncate
         //System.out.println("Sad width case");
         for (int i = 0; i < column_widths.length; i++)
         {
            column_widths[i] = (int)(((double)available_width)/((double)weights.length));
         }
      }

      // And now the heights
      if (available_height >= preferredSize.getHeight())
      {
         // Happy case
      }


      int x = 0;
      int y = 0;
      int required_height = 0;
      for (Integer i : row_heights)
         required_height += i;
      for (int row = 0; counted < componentCount; row++)
      {
         x = 0;
         int height = 0;
         if (available_height >= required_height)
         {
            height = row_heights.get(row);
         }
         else
         {
            height = (int)((double)available_height / (double)((double)componentCount / (double)weights.length));
         }
         for (int col = 0; col < weights.length; col++)
         {
            int width = column_widths[col];
            sortedComponents[counted++].setBounds(x, y, width, height);
            x = x + width;
         }
         y = y + height;
      }
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
