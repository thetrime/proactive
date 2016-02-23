import gnu.prolog.term.*;
import java.util.List;

public class PrologDocument extends PrologElement
{
   Term root;
   Term state;
   Term props;
   public PrologDocument(Term term, Term state, Term props) throws Exception
   {
      super(term);
      this.state = state;
      this.props = props;
   }
}
