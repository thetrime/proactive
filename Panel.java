import org.w3c.dom.*;
import javax.swing.*;
import java.awt.*;

public class Panel extends JPanel implements ReactComponent 
{
   private static final int HORIZONTAL = 0;
   private static final int VERTICAL = 1;
   int fill = GridBagConstraints.HORIZONTAL;
   int nextIndex = 0;
   int orientation = VERTICAL;
   public Panel(Node n)
   {
      setBackground(Color.GRAY);
      setLayout(new GridBagLayout());
      if ("horizontal".equals(((Element)n).getAttribute("layout")))
         orientation = HORIZONTAL;
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
         double xweight = 1.0;
         double yweight = 0;
         if (orientation == HORIZONTAL)
         {
            x = nextIndex;
            y = 0;
            yweight = 1.0;
            xweight = 0;
         }
         nextIndex++;
         System.out.println("x: " + x + ", y: " + y);
         add((Component)child, new GridBagConstraints(x, y, 1, 1, xweight, yweight, GridBagConstraints.NORTH, fill, new Insets(0,0,0,0), padx, pady));
      }
   }

   public void removeChild(ReactComponent child)
   {
      remove((Component)child);
   }
}
