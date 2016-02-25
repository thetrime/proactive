public class ReactEditWidget extends ReactEdit
{
   PrologNode patch;
   public ReactEditWidget(PrologNode node, PrologNode patch)
   {      
      super(node);
      this.patch = patch;
   }

   public ReactComponent apply(ReactComponent domNode) throws Exception
   {
      throw(new RuntimeException("WIDGET patch not implemented"));
   }
   
   public String toString()
   {
      return "<Mutate widget: " + node + ", " + patch + ">";
   }
}
