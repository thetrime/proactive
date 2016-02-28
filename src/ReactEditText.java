package org.proactive.vdom;
import org.proactive.ReactComponent;
import org.proactive.ReactComponentFactory;

public class ReactEditText extends ReactEdit
{
   PrologNode patch;
   public ReactEditText(PrologNode node, PrologNode patch)
   {      
      super(node);
      this.patch = patch;
   }

   public ReactComponent apply(ReactComponent domNode) throws Exception
   {
      /*
      if (domNode instanceof ReactTextComponent)
      {
         ((ReactTextComponent)domNode).replaceData(patch);
         return domNode;
      }
      */
      ReactComponent parentNode = domNode.getParentNode();
      if (parentNode != null)
      {
         ReactComponent newNode = ReactComponentFactory.instantiateNode(patch, parentNode.getContext());
         if (!newNode.equals(domNode))
            parentNode.replaceChild(newNode, domNode);
         return newNode;
      }
      return domNode;
   }
   
   public String toString()
   {
      return "<Mutate text: " + node + ", " + patch + ">";
   }
}
