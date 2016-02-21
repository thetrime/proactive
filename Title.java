import org.w3c.dom.*;
import javax.swing.*;

public class Title extends JLabel implements ReactComponent 
{
   public Title(Node n)
   {
      super(((Element)n).getAttribute("label"));
      /* This is if the child is the label.
        Node child = n.getFirstChild();
        if (child != null && child instanceof Text)
        setText(((Text)child).getWholeText());
      */
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling) {}
   public void removeChild(ReactComponent child) {}
}
