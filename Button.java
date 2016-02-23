import javax.swing.*;
import java.util.List;
import java.awt.GridBagConstraints;

public class Button extends JButton implements ReactComponent 
{
   private int fill = GridBagConstraints.NONE;
   private ReactComponent parent;
   private ReactComponent owner;
   public Button(PrologNode n)
   {      
   }
   public void insertChildBefore(ReactComponent child, ReactComponent sibling) {}
   public void removeChild(ReactComponent child) {}
   public ReactComponent getParentNode() { return parent; }
   public void setParentNode(ReactComponent parent) { this.parent = parent; }
   public ReactComponent getOwnerDocument() { return owner; }
   public void setOwnerDocument(ReactComponent owner) { this.owner = owner; }
   public void replaceChild(ReactComponent newChild, ReactComponent oldChild) {}
   public List<ReactComponent> getChildNodes() { return null; }
   public int getFill() { return fill; }
   public void setProperty(String name, Object value)
   {
      if (name.equals("label"))
         setText(Engine.asString(value));
      else if (name.equals("fill"))
         fill = React.getFill(value);
   }
}
