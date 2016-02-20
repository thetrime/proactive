import org.w3c.dom.*;
import javax.swing.*;

public class Title extends JLabel implements ReactComponent 
{
   public Title(Node n)
   {
      Node child = n.getFirstChild();
      if (child != null && child instanceof Text)
         setText(((Text)child).getWholeText());
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling) {}
   public void removeChild(ReactComponent child) {}
}
