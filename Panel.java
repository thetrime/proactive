import org.w3c.dom.*;
import javax.swing.*;
import java.awt.*;
import java.util.List;
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
   public Panel(String id)
   {
      this.id = id;
      setBackground(Color.RED);
      setLayout(layoutManager);
      fill = GridBagConstraints.BOTH;
      setBorder(BorderFactory.createLineBorder(Color.BLACK));
   }
   public Panel(Node n)
   {
      id = "<generated from " + n + ">";
      setBackground(Color.GRAY);
      setLayout(layoutManager);
      if ("horizontal".equals(((Element)n).getAttribute("layout")))
         orientation = HORIZONTAL;
      fill = React.getFill(n);
      for (Node child = n.getFirstChild(); child != null; child = child.getNextSibling())
      {
         ReactComponent component = React.instantiateNode(child);
         insertChildBefore(component, null);
      }
      setBorder(BorderFactory.createLineBorder(Color.BLACK));
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      // Remove from any previous child list!
      if (child.getParentNode() != null)
         child.getParentNode().removeChild(child);
      children.add(child);
      child.setParentNode(this);
      child.setOwnerDocument(owner);
      int index = -1;
      if (sibling != null)
      {
         // FIXME: This is definitely wrong
         for (int i = 0; i < getComponentCount(); i++)
         {
            if (getComponent(i) == sibling)
            {
               index = i-1;
               break;
            }
         }         
      }
      else
      {
         int padx = 0;
         int pady = 0;
         int x = 0;
         int y = nextIndex;
         int fill = child.getFill();
         double yweight = 0;
         double xweight = 0;
         if (fill == GridBagConstraints.HORIZONTAL || fill == GridBagConstraints.BOTH)
            yweight = 1;
         if (fill == GridBagConstraints.VERTICAL || fill == GridBagConstraints.BOTH)
            xweight = 1;
         
         if (orientation == HORIZONTAL)
         {
            x = nextIndex;
            y = 0;
         }


         
         nextIndex++;
         add((Component)child, new GridBagConstraints(x, y, 1, 1, xweight, yweight, GridBagConstraints.CENTER, fill, new Insets(0,0,0,0), padx, pady));
      }
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
      GridBagConstraints constraints = layoutManager.getConstraints((Component)oldChild);
      // We cannot call removeChild here since the list of children will get truncated
      // and we want to swap in-place
      children.set(i, newChild);
      remove((Component)oldChild);
      add((Component)newChild, constraints);
   }

   public List<ReactComponent> getChildNodes()
   {
      return children;
   }

   public int getFill() { return fill; }
}
