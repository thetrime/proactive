import org.w3c.dom.*;
import javax.swing.*;
import java.awt.*;

public class Panel extends JPanel implements ReactComponent 
{
   public Panel(Node n)
   {
      setBackground(Color.RED);
      for (Node child = n.getFirstChild(); child != null; child = child.getNextSibling())
      {
         ReactComponent c = React.instantiateNode(child);
         child.setUserData("dom", c, null);
         insertChildBefore(c, null);
      }
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      int index = -1;
      if (sibling != null)
      {
         // FIXME: This is probably wrong
         for (int i = 0; i < getComponentCount(); i++)
         {
            if (getComponent(i) == sibling)
            {
               index = i-1;
               break;
            }
         }         
      }
      add((Component)child, index);
   }

   public void removeChild(ReactComponent child)
   {
      remove((Component)child);
   }
}
