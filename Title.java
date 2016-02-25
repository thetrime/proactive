import javax.swing.*;
import java.util.List;
import java.awt.GridBagConstraints;

public class Title extends JLabel implements ReactComponent 
{
   private int fill = GridBagConstraints.NONE;
   private ReactComponent parent;
   private ReactComponent owner;
   private PrologContext context;
   public Title(PrologNode n, PrologContext context)
   {
      /* This is if the child is the label.
        Node child = n.getFirstChild();
        if (child != null && child instanceof Text)
        setText(((Text)child).getWholeText());
      */
      this.context = context;
   }
   public PrologContext getContext() {return context;}
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
