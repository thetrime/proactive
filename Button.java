import org.w3c.dom.*;
import javax.swing.*;

public class Button extends JButton implements ReactComponent 
{
   public Button(Node n)
   {
      super("Hello I am a button");
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling) {}
   public void removeChild(ReactComponent child) {}
}
