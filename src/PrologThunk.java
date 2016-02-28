public abstract class PrologThunk extends PrologNode
{
   public PrologNode vNode = null;
   public abstract PrologDocument render(PrologNode previous);
}
