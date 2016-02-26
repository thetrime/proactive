import javax.swing.*;
import java.util.List;
import java.awt.event.*;
import java.awt.Component;

public class Button extends ReactLeafComponent 
{
   private JButton button = new JButton();
   public Button(PrologNode n, PrologContext context)
   {
      super(context);
   }
   
   private ActionListener actionListener = null;
   public void setClickHandler(Object value)
   {
      if (actionListener != null)
         button.removeActionListener(actionListener);

      if (value == null)
         return;
      
      actionListener = new ActionListener()
         {
            public void actionPerformed(ActionEvent ae)
            {
               try
               {
                  context.triggerEvent(value);
               }
               catch (Exception e)
               {
                  e.printStackTrace();
               }
            }
         };
      button.addActionListener(actionListener);
   }
   
   public void setProperty(String name, Object value)
   {
      if (name.equals("label"))
         button.setText(Engine.asString(value));
      else if (name.equals("fill"))
         fill = getContext().getFill(value);
      else if (name.equals("onClick"))
         setClickHandler(value);
   }

   public Component getAWTComponent()
   {
      return button;
   }
}
