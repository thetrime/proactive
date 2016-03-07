package org.proactive.ui;

import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
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

   JPanel alignmentPanel = new JPanel();

   private java.util.List<ReactComponent> children = new LinkedList<ReactComponent>();
   private LayoutManager layoutManager = new GridBagLayout();
   private Component awtComponent;
   private JPanel panel = new JPanel();
   private String id;

   public Panel(String q) throws Exception
   {
      this();
      this.id = q;
   }

   public Panel() throws Exception
   {
      super();
      awtComponent = panel;
      panel.setBackground(new Color(150, 168, 200));
      panel.setLayout(layoutManager);
      alignmentPanel.setPreferredSize(new Dimension(0,0));
      //panel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
   }
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
      if (properties.containsKey("key"))
         id = properties.get("key").asString();
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
      child.setOwnerDocument(owner);
      int childFill = child.getFill();
      if (childFill == GridBagConstraints.HORIZONTAL || childFill == GridBagConstraints.BOTH)
         total_x_weight++;
      if (childFill == GridBagConstraints.VERTICAL || childFill == GridBagConstraints.BOTH)
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
         checkAlignment();
         addChildToDOM(nextIndex, child);
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
         int x = (orientation==VERTICAL)?0:(index+1);
         int y = (orientation==VERTICAL)?(index+1):0;
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
            panel.add(child.getAWTComponent(), new GridBagConstraints(x, y, 1, 1, xweight, yweight, anchor, childFill, new Insets(0,0,0,0), padx, pady));
         }
      }
   }
   
   private void repackChildren()
   {
      panel.removeAll();
      int index = 0;
      // This resets the alignment if the child can take over the job of gluing out the panel alignment
      checkAlignment();
      for (ReactComponent child: children)
         addChildToDOM(index++, child);
   }   

   public void checkAlignment()
   {
      if (alignment == END && fill != GridBagConstraints.NONE)
      {
         if (orientation == HORIZONTAL && total_x_weight == 0)
         {
            // Requires padding at left
            panel.add(alignmentPanel, new GridBagConstraints(0, 0, 1, 1, 1, 0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0,0,0,0), 0, 0));
         }
         else if (orientation == VERTICAL && total_y_weight == 0)
         {
            // Requires padding at top
            panel.add(alignmentPanel, new GridBagConstraints(0, 0, 1, 1, 0, 1, GridBagConstraints.CENTER, GridBagConstraints.VERTICAL, new Insets(0,0,0,0), 0, 0));
         }
         else
         {
            // Remove padding if present
            panel.remove(alignmentPanel);
         }
      }
      else if (alignment == START && fill != GridBagConstraints.NONE)
      {
         if (orientation == HORIZONTAL && total_x_weight == 0)
         {
            // Requires padding at right
            panel.add(alignmentPanel, new GridBagConstraints(children.size()+1, 0, 1, 1, 1, 0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0,0,0,0), 0, 0));
         }
         else if (orientation == VERTICAL && total_y_weight == 0)
         {
            // Requires padding at bottom
            panel.add(alignmentPanel, new GridBagConstraints(0, children.size()+1, 1, 1, 0, 1, GridBagConstraints.CENTER, GridBagConstraints.VERTICAL, new Insets(0,0,0,0), 0, 0));

         }
         else
         {
            // Remove padding if present
            panel.remove(alignmentPanel);
         }
      }
      else
         panel.remove(alignmentPanel);
   }

   public void removeChild(ReactComponent child)
   {
      children.remove(child);
      int childFill = child.getFill();
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
      int i = children.indexOf(oldChild);
      children.set(i, newChild);
      newChild.setParentNode(this);
      newChild.setOwnerDocument(owner);
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
         GridBagConstraints constraints = ((GridBagLayout)layoutManager).getConstraints(oldChild.getAWTComponent());
         // We cannot call removeChild here since the list of children will get truncated
         // and we want to swap in-place
         panel.remove(oldChild.getAWTComponent());
         // We may have to edit the constraints if the child has a different fill
         constraints.fill = newChild.getFill();
         panel.add(newChild.getAWTComponent(), constraints);
         checkAlignment();
      }
      else if (orientation == GRID)
      {
         panel.remove(oldChild.getAWTComponent());
         panel.add(newChild.getAWTComponent(), i);
      }
   }

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
      return "(Panel: " + id + ")";
   }
}
