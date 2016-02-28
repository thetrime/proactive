import javax.swing.*;
import java.awt.*;
import java.util.List;
import java.util.Iterator;
import java.util.LinkedList;

public class Panel extends ReactComponent 
{
   private static final int HORIZONTAL = 0;
   private static final int VERTICAL = 1;
   int nextIndex = 0;
   int orientation = VERTICAL;
   private java.util.List<ReactComponent> children = new LinkedList<ReactComponent>();
   private GridBagLayout layoutManager = new GridBagLayout();
   private JPanel panel = new JPanel();
   protected Panel()
   {
      super(null);
      panel.setLayout(layoutManager);
      panel.setBackground(Color.RED);
      fill = GridBagConstraints.BOTH;
      panel.setBorder(BorderFactory.createLineBorder(Color.BLUE));
   }
   public Panel(PrologNode n, PrologContext context) throws Exception
   {
      super(context);
      panel.setBackground(Color.GRAY);
      panel.setLayout(layoutManager);
      for (Iterator<PrologNode> i = n.getChildren().iterator(); i.hasNext();)
      {
         ReactComponent component = ReactComponentFactory.instantiateNode(i.next(), context);
         insertChildBefore(component, null);
      }
      panel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
   }
   public void setProperty(String name, Object value)
   {
      System.out.println("Setting " + name + " on " + this + " to " + value);
      if (name.equals("layout"))
      {
         int oldOrientation = orientation;
         if (value == null)
            orientation = VERTICAL;
         else if (value.equals("horizontal"))
            orientation = HORIZONTAL;
         else if (value.equals("vertical"))
            orientation = VERTICAL;
         if (orientation != oldOrientation)
            repackChildren();
      }
      else if (name.equals("fill"))
         fill = context.getFill(value);
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
      panel.add(child.getAWTComponent(), new GridBagConstraints(x, y, 1, 1, xweight, yweight, GridBagConstraints.CENTER, childFill, new Insets(0,0,0,0), padx, pady));
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
      GridBagConstraints constraints = layoutManager.getConstraints(oldChild.getAWTComponent());
      // We cannot call removeChild here since the list of children will get truncated
      // and we want to swap in-place
      children.set(i, newChild);
      panel.remove(oldChild.getAWTComponent());
      newChild.setParentNode(this);
      // We may have to edit the constraints if the child has a different fill
      constraints.fill = newChild.getFill();
      panel.add(newChild.getAWTComponent(), constraints);
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
