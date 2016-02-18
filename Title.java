import org.w3c.dom.*;
import javax.swing.*;

public class Title extends JLabel implements ReactComponent 
{
   public Title(Node n)
   {
      super("Hello, I am a label");
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling) {}
   public void removeChild(ReactComponent child) {}
}
