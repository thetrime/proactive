import gnu.prolog.term.*;
import java.util.List;

public class PrologDocument extends PrologElement
{
   Term root;
   PrologContext context;
   public PrologDocument(Term term, Term state, Term props, String componentName) throws Exception
   {
      super(term);
      this.context = new PrologContext(state, props, componentName, this);
   }
   public PrologContext getContext()
   {
      return context;
   }      
}
