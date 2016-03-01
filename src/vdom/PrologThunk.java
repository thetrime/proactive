package org.proactive.vdom;

public abstract class PrologThunk extends PrologNode
{
   public PrologNode vNode = null;
   public abstract PrologDocument render(PrologNode previous);

   @Override
   public boolean hasThunks()
   {
      return true;
   }
}
