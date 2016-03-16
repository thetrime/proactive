package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import org.proactive.ReactComponent;
import org.proactive.ReactComponentFactory;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JFrame;
import java.awt.GridBagLayout;
import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.GridBagConstraints;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Color;
import javax.swing.BorderFactory;
import javax.swing.border.TitledBorder;
import java.awt.Component;
import java.util.List;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.HashMap;

public class Panel extends ReactComponent 
{
   private static final int HORIZONTAL = 0;
   private static final int VERTICAL = 1;
   private static final int GRID = 2;
   private static final int START = 0;
   private static final int CENTER = 1;
   private static final int END = 2;
   int nextIndex = 0;
   int orientation = VERTICAL;
   int alignment = START;
   int total_x_weight = 0;
   int total_y_weight = 0;

   private LayoutManager layoutManager = new GridBagLayout();
   private Component awtComponent;
   private JPanel panel = new JPanel();
   private String id;
   private static int global_id = 0;

   ReactComponent alignmentComponent = null;
   LinkedList<ReactComponent> childComponents = new LinkedList<ReactComponent>();
   HashMap<ReactComponent, Integer> fillMap = new HashMap<ReactComponent, Integer>();
   public Panel(String q)
   {
      this();
      this.id = q;
   }

   public Panel()
   {
      super();
      this.id = "{" + (global_id++) + "}";
      awtComponent = panel;
      panel.setBackground(new Color(150, 168, 200));
      panel.setLayout(layoutManager);
      //panel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
   }
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
      if (properties.containsKey("key"))
      {
         id = properties.get("key").asString();
      }
      if (properties.containsKey("label"))
      {
         if (properties.get("label").isNull())
            panel.setBorder(null);
         else
         {
            java.awt.Font font = new java.awt.Font("Arial", 0, 11);
            java.awt.Color colour = java.awt.Color.WHITE;
            panel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(colour),
                                                                                                BorderFactory.createEmptyBorder(0, 0, 0, 0)),
                                                             properties.get("label").asString(),
                                                             TitledBorder.LEFT,
                                                             TitledBorder.TOP,
                                                             font,
                                                             colour));
         }
      }
      if (properties.containsKey("layout"))
      {
         int oldOrientation = orientation;
         if (properties.get("layout") == null)
         {
            orientation = VERTICAL;
         }
         else
         {
            String key = properties.get("layout").asOrientation();
            if (key.equals("vertical"))
               orientation = VERTICAL;
            else if (key.equals("horizontal"))
               orientation = HORIZONTAL;
            else if (key.equals("grid"))
               orientation = GRID;
            else
               orientation = VERTICAL;
         }
         if (orientation != oldOrientation)
         {
            if (oldOrientation == GRID && orientation != GRID)
            {
               layoutManager = new GridBagLayout();
               panel.setLayout(layoutManager);
            }
            else if (oldOrientation != GRID && orientation == GRID)
            {
               int rows = 0;
               int cols = 0;
               if (properties.containsKey("cols"))
                  cols = properties.get("cols").asInteger();
               if (properties.containsKey("rows"))
                  rows = properties.get("rows").asInteger();
               layoutManager = new GridLayout(rows, cols);
               panel.setLayout(layoutManager);
            }
            repackChildren();
         }
      }
      if (properties.containsKey("align-children"))
      {
         int oldAlignment = alignment;
         if (properties.get("align-children") == null)
         {
            alignment = START;
         }
         else
         {
            String key = properties.get("align-children").asString();
            if (key.equals("start"))
                alignment = START;
            else if (key.equals("center"))
                alignment = CENTER;
            if (key.equals("end"))
               alignment = END;
         }
         if (oldAlignment != alignment)
            repackChildren();
      }
      if (properties.containsKey("scroll"))
      {
         // FIXME: Not this simple!
         //   * Check scroll modes
         //   * Could be turning scroll OFF!
         //   * Could be changing from scroll->different scroll
         if (properties.get("scroll") == null)
         {
            awtComponent = panel;
         }
         else
         {
            String key = properties.get("scroll").asScroll();
            JScrollPane scroll = new JScrollPane(panel);
            if (key.equals("vertical"))
            {
               scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
               scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
            }
            else if (key.equals("horizontal"))
            {
               scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
               scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
            }
            else if (key.equals("both"))
            {
               scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
               scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            }
            awtComponent = scroll;
         }
         if (getParentNode() != null)
            getParentNode().replaceChild(this, this);
      }
   }
   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      // First rehome the child in the document
      if (child.getParentNode() != null)
         child.getParentNode().removeChild(child);
      child.setParentNode(this);
      awtMap.put(child, child.getAWTComponent());
      fillMap.put(child, child.getFill());
      int childFill = child.getFill();
      if ((childFill == GridBagConstraints.HORIZONTAL) || (childFill == GridBagConstraints.BOTH))
         total_x_weight++;
      if ((childFill == GridBagConstraints.VERTICAL) || (childFill == GridBagConstraints.BOTH))
         total_y_weight++;
      int index = (sibling==null)?-1:children.indexOf(sibling);
      if (index != -1)
      {
         children.add(index, child);
         repackChildren();
      }
      else
      {
         children.add(child);
         addChildToDOM(nextIndex, child);
         checkAlignment();
         nextIndex++;         
      }
   }

   private void addChildToDOM(int index, ReactComponent child)
   {
      if (orientation == GRID)
      {
         if (!(child.getAWTComponent() instanceof JFrame))
            panel.add(child.getAWTComponent(), index);
      }
      else
      {
         int padx = 0;
         int pady = 0;
         int x = (orientation==VERTICAL)?0:(index);
         int y = (orientation==VERTICAL)?(index):0;
         int childFill = child.getFill();
         double yweight = 0;
         double xweight = 0;
         int anchor = GridBagConstraints.CENTER;
         if (childFill == GridBagConstraints.HORIZONTAL || childFill == GridBagConstraints.BOTH)
            xweight = 1;
         if (childFill == GridBagConstraints.VERTICAL || childFill == GridBagConstraints.BOTH)
            yweight = 1;
         if (!(child.getAWTComponent() instanceof JFrame))
         {
            childComponents.add(child);
            panel.add(child.getAWTComponent(), new GridBagConstraints(x, y, 1, 1, xweight, yweight, anchor, childFill, new Insets(0,0,0,0), padx, pady));
         }
      }
   }
   
   private void repackChildren()
   {
      panel.removeAll();
      childComponents.clear();
      int index = 0;
      // This resets the alignment if the child can take over the job of gluing out the panel alignment
      checkAlignment();
      for (ReactComponent child: children)
         addChildToDOM(index++, child);
   }

   // Returns true if we changed the component
   private boolean setComponentConstraints(ReactComponent child, int requiredFill, int requiredAnchor, double requiredWeightX, int requiredWeightY)
   {
      Component oldComponent = child.getAWTComponent();
      GridBagConstraints constraints = ((GridBagLayout)layoutManager).getConstraints(oldComponent);
      if (constraints.fill == requiredFill &&
          constraints.anchor == requiredAnchor &&
          constraints.weightx == requiredWeightX &&
          constraints.weighty == requiredWeightY)
         return false;
      panel.remove(oldComponent);
      constraints.fill = requiredFill;
      constraints.anchor = requiredAnchor;
      constraints.weighty = requiredWeightY;
      constraints.weightx = requiredWeightX;
      panel.add(oldComponent, constraints);
      return true;
   }

   private void resetAlignmentComponent()
   {
      if (alignmentComponent != null)
      {
         setComponentConstraints(alignmentComponent,
                                 alignmentComponent.getFill(),
                                 GridBagConstraints.CENTER,
                                 (alignmentComponent.getFill() == GridBagConstraints.HORIZONTAL || alignmentComponent.getFill() == GridBagConstraints.BOTH)?1:0,
                                 (alignmentComponent.getFill() == GridBagConstraints.VERTICAL || alignmentComponent.getFill() == GridBagConstraints.BOTH)?1:0);
         alignmentComponent = null;
      }
   }

   public void checkAlignment()
   {
      if (childComponents.size() == 0 || orientation == GRID)
         return;
      if (alignment == END && fill != GridBagConstraints.NONE)
      {
         if (orientation == HORIZONTAL && total_x_weight == 0)
         {
            // Requires padding at left. Make first element wide
                        resetAlignmentComponent();
            int fill = childComponents.getFirst().getFill();
            if (setComponentConstraints(childComponents.getFirst(), fill, GridBagConstraints.EAST, 1, (childComponents.getFirst().getFill() == GridBagConstraints.VERTICAL || childComponents.getFirst().getFill() == GridBagConstraints.BOTH)?1:0))
               alignmentComponent = childComponents.getFirst();
         }
         else if (orientation == VERTICAL && total_y_weight == 0)
         {
            // Requires padding at top. Make first element tall
            resetAlignmentComponent();
            int fill = childComponents.getFirst().getFill();
            if (setComponentConstraints(childComponents.getFirst(), fill, GridBagConstraints.SOUTH, (childComponents.getFirst().getFill() == GridBagConstraints.HORIZONTAL || childComponents.getFirst().getFill() == GridBagConstraints.BOTH)?1:0, 1))
               alignmentComponent = childComponents.getFirst();
         }
         else if (alignmentComponent != null)
         {
            // Remove padding if present from alignment component
            resetAlignmentComponent();
         }
      }
      else if (alignment == START && fill != GridBagConstraints.NONE)
      {
         if (orientation == HORIZONTAL && total_x_weight == 0)
         {
            // Requires padding at right
            resetAlignmentComponent();
            int fill = childComponents.getLast().getFill();
            if (setComponentConstraints(childComponents.getLast(), fill, GridBagConstraints.WEST, 1, (childComponents.getLast().getFill() == GridBagConstraints.VERTICAL || childComponents.getLast().getFill() == GridBagConstraints.BOTH)?1:0))
               alignmentComponent = childComponents.getLast();
         }
         else if (orientation == VERTICAL && total_y_weight == 0)
         {
            // Requires padding at bottom
            resetAlignmentComponent();
            int fill = childComponents.getLast().getFill();
            if (setComponentConstraints(childComponents.getLast(), fill, GridBagConstraints.NORTH, (childComponents.getLast().getFill() == GridBagConstraints.HORIZONTAL || childComponents.getLast().getFill() == GridBagConstraints.BOTH)?1:0, 1))
               alignmentComponent = childComponents.getLast();
         }
         else if (alignmentComponent != null)
         {
            // Remove padding if present from last element
            resetAlignmentComponent();
         }
      }
      else if (alignmentComponent != null)
      {
         // Remove any added padding from first AND last elements
         resetAlignmentComponent();
      }
   }

   public void removeChild(ReactComponent child)
   {
      if (!children.contains(child))
         return;
      children.remove(child);
      childComponents.remove(child);
      awtMap.remove(child);
      int childFill = fillMap.get(child);
      fillMap.remove(child);
      if (childFill == GridBagConstraints.HORIZONTAL || childFill == GridBagConstraints.BOTH)
         total_x_weight--;
      if (childFill == GridBagConstraints.VERTICAL || childFill == GridBagConstraints.BOTH)
         total_y_weight--;
      nextIndex--;
      checkAlignment();
      panel.remove(child.getAWTComponent());
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      // If the component is the same, do not remove and replace it; doing so will only
      // cause it to lose focus for no reason
      if (newChild.getAWTComponent().equals(oldChild.getAWTComponent()) &&
          fillMap.get(newChild).intValue() == newChild.getFill())
         return;

      int i = children.indexOf(oldChild);
      Component oldComponent = awtMap.get(oldChild);
      childComponents.remove(oldChild);
      fillMap.remove(oldChild);
      fillMap.put(newChild, newChild.getFill());

      super.replaceChild(newChild, oldChild);
      int childFill = oldChild.getFill();
      if (childFill == GridBagConstraints.HORIZONTAL || childFill == GridBagConstraints.BOTH)
         total_x_weight--;
      if (childFill == GridBagConstraints.VERTICAL || childFill == GridBagConstraints.BOTH)
         total_y_weight--;
      childFill = newChild.getFill();
      if (childFill == GridBagConstraints.HORIZONTAL || childFill == GridBagConstraints.BOTH)
         total_x_weight--;
      if (childFill == GridBagConstraints.VERTICAL || childFill == GridBagConstraints.BOTH)
         total_y_weight--;
      if (orientation == VERTICAL || orientation == HORIZONTAL)
      {
         GridBagConstraints constraints = ((GridBagLayout)layoutManager).getConstraints(oldComponent);
         // We cannot call removeChild here since the list of children will get truncated
         // and we want to swap in-place
         panel.remove(oldComponent);
         // We may have to edit the constraints if the child has a different fill
         constraints.fill = newChild.getFill();
         if (!(newChild.getAWTComponent() instanceof JFrame))
         {
            panel.add(newChild.getAWTComponent(), constraints);
            checkAlignment();
         }
      }
      else if (orientation == GRID)
      {
         panel.remove(oldComponent);
         if (!(newChild.getAWTComponent() instanceof JFrame))
         {
            panel.add(newChild.getAWTComponent(), i);
         }
      }
   }

  // FIXME: Is this necessary? Doesnt ReactComponent do this?
   public List<ReactComponent> getChildNodes()
   {
      return children;
   }

   public Component getAWTComponent()
   {
      return awtComponent;
   }

   public String toString()
   {
      if (id != null)
         return "(Panel: " + id + ")";
      return "(Panel " + children + ")";
   }
}
