public class ReactEditRemove extends ReactEdit
{
   public ReactEditRemove(PrologNode node, PrologNode ignored)
   {
      super(node);
   }

   public ReactComponent apply(ReactComponent domNode) throws Exception
   {
      ReactComponent parentNode = domNode.getParentNode();
      if (parentNode != null)
         parentNode.removeChild(domNode);
      return null;
   }
   public String toString()
   {
      return "<Remove node: " + node + ">";
   }
}
