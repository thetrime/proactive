package org.proactive.vdom;
import org.proactive.ReactComponent;

public class ReactEditThunk extends ReactEdit
{
   PatchSet patch;
   public ReactEditThunk(PrologNode node, PatchSet patch)
   {      
      super(node);
      this.patch = patch;
   }

   public ReactComponent apply(ReactComponent domNode) throws Exception
   {
      ReactComponent newRoot = patch.apply(domNode);
      if (domNode != null && newRoot != null && !(newRoot.equals(domNode)) && domNode.getParentNode() != null)
         domNode.getParentNode().replaceChild(newRoot, domNode);
      return newRoot;
   }
   
   public String toString()
   {
      return "<Mutate thunk: " + node + ", " + patch + ">";
   }
}
