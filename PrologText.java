import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;


public class PrologText extends PrologNode
{
   String text;
   public PrologText(Term t) throws Exception
   {
      if (!(t instanceof AtomTerm))
         throw new RuntimeException("Invalid XML: Text is not text");
      text = ((AtomTerm)t).value;
   }
   public String getWholeText()
   {
      return text;
   }
}
