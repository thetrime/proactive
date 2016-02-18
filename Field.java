import org.w3c.dom.*;
import javax.swing.*;

public class Field extends JTextField implements ReactComponent 
{
   public Field(Node n)
   {
      super("A field");
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling) {}
   public void removeChild(ReactComponent child) {}
}
