import org.w3c.dom.*;

// There are 6 edit operations that React permits:

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
   public static final int REMOVE = 7;   // Delete node (and children0
   public static final int THUNK = 8;    // ??
   Object node;
   Object patch;
   
   public ReactEdit(int op, Object node, Object patch)
   {
      this.op = op;
      this.node = node;
      this.patch = patch;
   }
   
   public String toString()
   {
      switch(op)
      {
         case INSERT:
            return "<Insert: " + node + ">";
         case NODE:
            return "<Mutate node: " + node + ", " + patch + ">";
      }
      return "<unknown operation " + op + ">";
   }

   public ReactComponent apply(ReactComponent domNode)
   {
      switch(op)
      {
         case NODE:
         {
            ReactComponent parentNode = domNode.getParentNode();
            ReactComponent newNode = React.instantiateNode((Node)patch);
            if (parentNode != null)
            {
               parentNode.replaceChild(newNode, domNode);
            }
            return newNode;
         }
         case REMOVE:
         {
            ReactComponent parentNode = domNode.getParentNode();
            if (parentNode != null)
               parentNode.removeChild(domNode);
            break;
         }
         default:
            System.out.println("Unimplemented patch op " + op);
         
      }
      return null;
   }
   
}

