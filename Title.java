import javax.swing.*;
import java.util.List;
import java.awt.Component;

public class Title extends ReactLeafComponent 
{
   JLabel label = new JLabel();
   public Title(PrologNode n, PrologContext context)
   {
      super(context);
      /* This is if the child is the label.
        Node child = n.getFirstChild();
        if (child != null && child instanceof Text)
        setText(((Text)child).getWholeText());
      */
   }
   public void setProperty(String name, Object value)
   {
      if (name.equals("label"))
         label.setText(Engine.asString(value));
      else if (name.equals("fill"))
         fill = context.getFill(value);
   }
   public Component getAWTComponent()
   {
      return label;
   }
}
