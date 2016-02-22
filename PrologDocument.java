import gnu.prolog.term.*;
import java.util.List;

public class PrologDocument extends PrologElement
{
   Term root;
   public PrologDocument(Term term) throws Exception
   {
      super(term);
   }

   public PrologDocument() throws Exception
   {
      super();
      root = null;
   }

   public PrologNode getFirstChild()
   {
      if (children.size() == 0)
         return null;
      return children.get(0);
   }
   
}
