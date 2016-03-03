package org.proactive.vdom;
import org.proactive.ReactComponent;
import org.proactive.ReactComponentFactory;

public class ReactEditWidget extends ReactEdit
{
   PrologWidget patch;
   public ReactEditWidget(PrologNode node, PrologWidget patch)
   {      
      super(node);
      this.patch = patch;
   }

   public ReactComponent apply(ReactComponent domNode) throws Exception
   {
      boolean updating = false;
      if (node instanceof PrologWidget)
      {
         // if ((PrologWidget)node).getAttribute('name') != null && patch.getAttribute('name') != null)
         //    updating = a.id == b.id
         // else
         //    updating = a.init == b.init
      }
      System.out.println("Applying widget patch: " + javax.swing.SwingUtilities.isEventDispatchThread());
      ReactComponent newNode;
      ReactComponent parentNode = domNode.getParentNode();
      if (updating)
      {
         newNode = patch.update(node, domNode);
         if (newNode == null)
            newNode = domNode;
      }
      else
      {
         newNode = ReactComponentFactory.instantiateNode(patch, parentNode.getContext());
      }
      if (parentNode != null && newNode != domNode)
      {
         parentNode.replaceChild(newNode, domNode);
         parentNode.getAWTComponent().validate();
      }
      if (!updating)
         destroyWidget(domNode, node);
      return newNode;
   }
   
   public String toString()
   {
      return "<Mutate widget: " + node + ", " + patch + ">";
   }
}
