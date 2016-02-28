package org.proactive.vdom;

import org.proactive.ReactComponent;
import org.proactive.ReactComponentFactory;

public class ReactEditNode extends ReactEdit
{
   PrologNode patch;
   public ReactEditNode(PrologNode node, PrologNode patch)
   {
      super(node);
      this.patch = patch;
   }
   public ReactComponent apply(ReactComponent domNode) throws Exception
   {
      ReactComponent parentNode = domNode.getParentNode();
      // I think this is right. If the parent has a different execution context, then the child will create a completely
      // new context in instantiateNode() and this will be ignored
      ReactComponent newNode = ReactComponentFactory.instantiateNode(patch, parentNode.getContext());
      if (parentNode != null)
      {
         parentNode.replaceChild(newNode, domNode);
      }
      return newNode;
   }
   public String toString()
   {
      return "<Mutate node: " + node + ", " + patch + ">";
   }
}
