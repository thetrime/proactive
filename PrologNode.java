import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;

public class PrologNode
{
   String nodeName;
   Map<String, Object> attributes = new HashMap<String, Object>();
   List<PrologNode> children = new LinkedList<PrologNode>();
  
   public String getNodeName()
   {
      return nodeName;
   }

   public Map<String, Object> getAttributes()
   {
      return attributes;
   }

   public List<PrologNode> getChildren()
   {
      return children;
   }

   public static PrologNode instantiateNode(Term t) throws Exception
   {
      PrologNode result = null;
      if (t instanceof AtomTerm)
         result = new PrologText(t);
      else
         result = new PrologElement(t);
      return result;
   }
}
