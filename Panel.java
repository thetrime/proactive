import javax.swing.*;
import java.awt.*;
import java.util.List;
import java.util.Iterator;
import java.util.LinkedList;

public class Panel extends JPanel implements ReactComponent 
{
   private static final int HORIZONTAL = 0;
   private static final int VERTICAL = 1;
   int nextIndex = 0;
   int orientation = VERTICAL;
   private ReactComponent parent = null;
   private ReactComponent owner = null;
   private java.util.List<ReactComponent> children = new LinkedList<ReactComponent>();
   private GridBagLayout layoutManager = new GridBagLayout();
   private String id;
   private int fill = GridBagConstraints.NONE;
   protected PrologContext context;

   protected Panel()
   {
      this.id = "<default node>";
      setLayout(layoutManager);
      setBackground(Color.RED);
      fill = GridBagConstraints.BOTH;
      setBorder(BorderFactory.createLineBorder(Color.BLUE));
   }
   public Panel(String id)
   {
      this.id = id;
      setBackground(Color.RED);
      setLayout(layoutManager);
      fill = GridBagConstraints.BOTH;
      setBorder(BorderFactory.createLineBorder(Color.BLACK));
   }
   public Panel(PrologNode n, PrologContext context) throws Exception
   {
      id = "<generated from " + n + ">";
      setBackground(Color.GRAY);
      setLayout(layoutManager);
      for (Iterator<PrologNode> i = n.getChildren().iterator(); i.hasNext();)
      {
         ReactComponent component = ReactComponentFactory.instantiateNode(i.next(), context);
         insertChildBefore(component, null);
      }
      setBorder(BorderFactory.createLineBorder(Color.BLACK));
      this.context = context;
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
         fill = React.getFill(value);
   }
   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      // Remove from any previous child list first
      if (child.getParentNode() != null)
         child.getParentNode().removeChild(child);
      children.add(child);
      child.setParentNode(this);
      child.setOwnerDocument(owner);
      int index = -1;
      if (sibling != null)
      {
         // FIXME: This is definitely wrong
         System.out.println("THIS IS WRONG");
         System.exit(-1);
      }
      else
      {
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
      add((Component)child, new GridBagConstraints(x, y, 1, 1, xweight, yweight, GridBagConstraints.CENTER, childFill, new Insets(0,0,0,0), padx, pady));
   }
   
   private void repackChildren()
   {
      removeAll();
      int index = 0;
      for (Iterator<ReactComponent> i = children.iterator(); i.hasNext();)
         addChildToDOM(index++, i.next());
   }
   
   public String toString()
   {
      return "Panel[" + id + "]";
   }

   public void removeChild(ReactComponent child)
   {
      children.remove(child);
      remove((Component)child);
   }

   public ReactComponent getParentNode()
   {
      return parent;
   }

   public void setParentNode(ReactComponent parent)
   {
      this.parent = parent;
   }

   public ReactComponent getOwnerDocument()
   {
      return owner;
   }

   public void setOwnerDocument(ReactComponent owner)
   {
      this.owner = owner;
      // FIXME: Also set on all children!
   }

   
   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      int i = children.indexOf(oldChild);
      System.out.println("Created: " + newChild);
      GridBagConstraints constraints = layoutManager.getConstraints((Component)oldChild);
      // We cannot call removeChild here since the list of children will get truncated
      // and we want to swap in-place
      children.set(i, newChild);
      remove((Component)oldChild);
      newChild.setParentNode(this);
      // We may have to edit the constraints if the child has a different fill
      constraints.fill = newChild.getFill();
      add((Component)newChild, constraints);
   }

   public List<ReactComponent> getChildNodes()
   {
      return children;
   }

   public int getFill() { return fill; }
   public PrologContext getContext() {return context;}
}
