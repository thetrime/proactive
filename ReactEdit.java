import java.util.Set;
import java.util.Map;
import java.util.Iterator;

// There are 9 edit operations that React permits:

public class ReactEdit
{
   int op;
   public static final int NONE = 0;     // No-op
   public static final int TEXT = 1;     // Mutate text
   public static final int NODE = 2;     // Mutate tag
   public static final int WIDGET = 3;   // ???
   public static final int PROPS = 4;    // Mutate properties
   public static final int ORDER = 5;    // Change order
   public static final int INSERT = 6;   // Insert node (and children)
   public static final int REMOVE = 7;   // Delete node (and children)
   public static final int THUNK = 8;    // ??
   PrologNode node;
   
   public ReactEdit(PrologNode node)
   {
      this.node = node;
   }

   public ReactEdit(int op, PrologNode node, Object patch)
   {
      this.op = op;
      this.node = node;
   }   
   
   public String toString()
   {
      switch(op)
      {
         case INSERT:
            return "<Insert: " + node + ">";
      }
      return "<unknown operation " + op + ">";
   }

   public ReactComponent apply(ReactComponent domNode) throws Exception
   {      
      System.out.println("Unimplemented patch op " + op);
      return null;
   }

  
   
}

