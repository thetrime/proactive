package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import org.proactive.ReactComponent;
import org.proactive.ReactComponentFactory;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JFrame;
import java.awt.Dimension;
import java.awt.LayoutManager;
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

   private LayoutManager layoutManager = new ProactiveLayoutManager(ProactiveLayoutManager.VERTICAL);
   private Component awtComponent;
   private JPanel panel = new JPanel();
   private String id;
   private static int global_id = 0;

   ReactComponent alignmentComponent = null;
   LinkedList<ReactComponent> childComponents = new LinkedList<ReactComponent>();
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
            if (orientation != GRID)
            {
               if (orientation == HORIZONTAL)
                  layoutManager = new ProactiveLayoutManager(ProactiveLayoutManager.HORIZONTAL);
               else if (orientation == VERTICAL)
                  layoutManager = new ProactiveLayoutManager(ProactiveLayoutManager.VERTICAL);
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
         nextIndex++;
      }

      child.setOwnerDocument(owner);
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
         if (!(child.getAWTComponent() instanceof JFrame))
         {
            childComponents.add(child);
            panel.add(child.getAWTComponent(), new ProactiveConstraints(child.getFill(), index));
         }
      }
   }
   
   private void repackChildren()
   {
      panel.removeAll();
      childComponents.clear();
      int index = 0;
      for (ReactComponent child: children)
         addChildToDOM(index++, child);
   }


   public void removeChild(ReactComponent child)
   {
      // FIXME: This is wallpaper. We should not be removing children from things that are not their parents
      //        I suspect this happens in things like Frame where adding a child to the Frame actually adds it
      //        to the Panel inside the Frame
      if (!children.contains(child))
         return;
      child.setOwnerDocument(null);
      children.remove(child);
      childComponents.remove(child);
      child.setParentNode(null);
      awtMap.remove(child);
      panel.remove(child.getAWTComponent());
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      int i = children.indexOf(oldChild);
      children.set(i, newChild);
      Component oldComponent = awtMap.get(oldChild);
      childComponents.remove(oldChild);
      oldChild.setOwnerDocument(null);
      oldChild.setParentNode(null);
      newChild.setOwnerDocument(owner);
      newChild.setParentNode(this);
      awtMap.remove(oldChild);
      awtMap.put(newChild, newChild.getAWTComponent());
      if (orientation == VERTICAL || orientation == HORIZONTAL)
      {
         ProactiveConstraints constraints = ((ProactiveLayoutManager)layoutManager).getConstraints(oldComponent);
         // We cannot call removeChild here since the list of children will get truncated
         // and we want to swap in-place
         panel.remove(oldComponent);
         // We may have to edit the constraints if the child has a different fill
         constraints.fill = newChild.getFill();
         if (!(newChild.getAWTComponent() instanceof JFrame))
         {
            panel.add(newChild.getAWTComponent(), constraints);
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

   public Component getAWTComponent()
   {
      return awtComponent;
   }

   public String toString()
   {
      if (id != null)
         return "(Panel: " + id + ", " + children + ")";
      return "(Panel " + children + ")";
   }
}
