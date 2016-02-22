import gnu.prolog.term.*;

public class PrologDocument extends PrologElement
{
   Term root;
   public PrologDocument(Term term) throws Exception
   {
      super();
      children.add(PrologNode.instantiateNode(term));
   }

   public PrologDocument() throws Exception
   {
      super();
      root = null;
   }
}
