import javax.swing.*;
import java.util.List;
import java.awt.Component;

public class Field extends ReactLeafComponent 
{
   private JTextField field = new JTextField("A field");
   public Field(PrologNode n, PrologContext context)
   {
      super(context);
   }
   public void setProperty(String name, Object value)
   {
      if (name.equals("fill"))
         fill = context.getFill(value);
   }
   public Component getAWTComponent()
   {
      return field;
   }
}
