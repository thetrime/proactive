import java.util.*;

public class ReactEditProps extends ReactEdit
{
   Map<String,Object> patch;
   public ReactEditProps(PrologNode node, Map<String,Object> patch)
   {
      super(node);
      this.patch = patch;
   }

   public ReactComponent apply(ReactComponent domNode) throws Exception
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
      return null;
   }

   public String toString()
   {
      return "<Mutate props: " + patch + ">";
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
