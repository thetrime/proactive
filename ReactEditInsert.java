public class ReactEditInsert extends ReactEdit
{
   public ReactEditInsert(PrologNode node)
   {      
      super(node);
   }

   public ReactComponent apply(ReactComponent domNode) throws Exception
   {
      if (domNode != null)
      {      
         ReactComponent newNode = ReactComponentFactory.instantiateNode(node, domNode.getContext());
         domNode.insertChildBefore(newNode, null);
      }
      return domNode;
   }
   
   public String toString()
   {
      return "<Insert element: " + node + ">";
   }
}
