import javax.swing.*;
import java.util.List;
import java.awt.event.*;
import java.awt.GridBagConstraints;

public class Button extends JButton implements ReactComponent 
{
   private int fill = GridBagConstraints.NONE;
   private ReactComponent parent;
   private ReactComponent owner;
   private PrologContext context;
   public Button(PrologNode n, PrologContext context)
   {
      this.context = context;
   }
   public PrologContext getContext() {return context;}
   public void insertChildBefore(ReactComponent child, ReactComponent sibling) {}
   public void removeChild(ReactComponent child) {}
   public ReactComponent getParentNode() { return parent; }
   public void setParentNode(ReactComponent parent) { this.parent = parent; }
   public ReactComponent getOwnerDocument() { return owner; }
   public void setOwnerDocument(ReactComponent owner) { this.owner = owner; }
   public void replaceChild(ReactComponent newChild, ReactComponent oldChild) {}
   public List<ReactComponent> getChildNodes() { return null; }
   public int getFill() { return fill; }

   private ActionListener actionListener = null;
   public void setClickHandler(Object value)
   {
      if (actionListener != null)
         removeActionListener(actionListener);

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
      addActionListener(actionListener);
   }
   
   public void setProperty(String name, Object value)
   {
      if (name.equals("label"))
         setText(Engine.asString(value));
      else if (name.equals("fill"))
         fill = getContext().getFill(value);
      else if (name.equals("onClick"))
         setClickHandler(value);
   }
}
