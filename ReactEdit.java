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
   Object patch;
   
   public ReactEdit(int op, PrologNode node, Object patch)
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
         case REMOVE:
            return "<Remove node: " + patch + ">";
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
            ReactComponent newNode = React.instantiateNode((PrologNode)patch);
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
         case PROPS:
         {
            Set<Map.Entry<String, Object>> entries = ((Map<String,Object>)patch).entrySet();
            Map<String,Object> previous = node.getAttributes();
            for (Iterator<Map.Entry<String, Object>> i = entries.iterator(); i.hasNext();)
            {
               Map.Entry<String, Object> entry = i.next();
               String propName = entry.getKey();
               Object propValue = entry.getValue();
               if (propValue == null)
               {  // removeProperty
                  if (previous != null)
                  {
                     Object previousValue = previous.get(propName);
                     if (!isHook(previousValue))
                     {
                        if (propName.equals("attributes"))
                        {
                        }
                        else if (propName.equals("style"))
                        {
                        }
                        else if (previousValue instanceof String)
                           domNode.setProperty(propName, "");
                        else
                           domNode.setProperty(propName, null);
                     }
                     else
                        ((Hook)previousValue).unHook(domNode, propName, propValue);
                  }
               }
               else // FIXME: Missing case here for isHook and isObject
               {
                  domNode.setProperty(propName, propValue);
               }
            }
         }
         default:
            System.out.println("Unimplemented patch op " + op);
         
      }
      return null;
   }

   private static class Hook
   {
      public void unHook(ReactComponent c, String name, Object value) {}
   }
   private static boolean isHook(Object anything)
   {
      return false;
   }
   
}

