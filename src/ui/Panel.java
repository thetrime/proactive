package org.proactive.ui;

import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import org.proactive.ReactComponent;
import org.proactive.ReactComponentFactory;

import javax.swing.JPanel;
import javax.swing.JFrame;
import java.awt.GridBagLayout;
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
   int nextIndex = 0;
   int orientation = VERTICAL;
   private java.util.List<ReactComponent> children = new LinkedList<ReactComponent>();
   private LayoutManager layoutManager = new GridBagLayout();
   private JPanel panel = new JPanel();

   public Panel(PrologNode n, PrologContext context) throws Exception
   {
      super(context);
      panel.setBackground(Color.GRAY);
      panel.setLayout(layoutManager);
      panel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
   }
   public void setProperties(HashMap<String, PrologObject> properties)
   {
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
               if (properties.containsKey("rows"))
                  rows = properties.get("rows").asInteger();
               if (properties.containsKey("cols"))
                  cols = properties.get("cols").asInteger();
               layoutManager = new GridLayout(rows, cols);
               panel.setLayout(layoutManager);

            }
            repackChildren();
         }
      }
      if (properties.containsKey("fill"))
      {
         fill = properties.get("fill").asFill();
      }
   }
   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      // First rehome the child in the document
      if (child.getParentNode() != null)
         child.getParentNode().removeChild(child);
      child.setParentNode(this);
      child.setOwnerDocument(owner);
      
      // Now insert the visual component
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
         int x = (orientation==VERTICAL)?0:index;
         int y = (orientation==VERTICAL)?index:0;
         int childFill = child.getFill();
         double yweight = 0;
         double xweight = 0;
         if (childFill == GridBagConstraints.HORIZONTAL || childFill == GridBagConstraints.BOTH)
            xweight = 1;
         if (childFill == GridBagConstraints.VERTICAL || childFill == GridBagConstraints.BOTH)
            yweight = 1;
         if (!(child.getAWTComponent() instanceof JFrame))
            panel.add(child.getAWTComponent(), new GridBagConstraints(x, y, 1, 1, xweight, yweight, GridBagConstraints.CENTER, childFill, new Insets(0,0,0,0), padx, pady));
      }
   }
   
   private void repackChildren()
   {
      panel.removeAll();
      int index = 0;
      for (Iterator<ReactComponent> i = children.iterator(); i.hasNext();)
         addChildToDOM(index++, i.next());
   }   

   public void removeChild(ReactComponent child)
   {
      children.remove(child);
      panel.remove(child.getAWTComponent());
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      int i = children.indexOf(oldChild);
      children.set(i, newChild);
      newChild.setParentNode(this);
      if (orientation == VERTICAL || orientation == HORIZONTAL)
      {
         GridBagConstraints constraints = ((GridBagLayout)layoutManager).getConstraints(oldChild.getAWTComponent());
         // We cannot call removeChild here since the list of children will get truncated
         // and we want to swap in-place
         panel.remove(oldChild.getAWTComponent());
         // We may have to edit the constraints if the child has a different fill
         constraints.fill = newChild.getFill();
         panel.add(newChild.getAWTComponent(), constraints);
      }
      else if (orientation == GRID)
      {
         panel.add(newChild.getAWTComponent(), i);
      }
   }

   public List<ReactComponent> getChildNodes()
   {
      return children;
   }

   public Component getAWTComponent()
   {
      return panel;
   }
}
