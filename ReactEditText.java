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
      // FIXME: Implement!
      throw(new RuntimeException("TEXT patch not implemented"));
   }
   
   public String toString()
   {
      return "<Mutate text: " + node + ", " + patch + ">";
   }
}
