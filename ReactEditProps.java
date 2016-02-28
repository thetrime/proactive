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
         {
            removeProperty(domNode, propName, propValue, previous);
         }
         else if (isHook(propValue))
         {
            removeProperty(domNode, propName, propValue, previous);
            if (((Hook)propValue).hook != null)
            {
               ((Hook)propValue).hook.somethingChanged(domNode, propName, previous!=null?previous.get(propName) : null);
            }
         }
         else
         {
            // setProperty expects an object and sorts that out itself, so we dont need the isObject() case
            domNode.setProperty(propName, propValue);
         }
      }
      return null;
   }

   private void removeProperty(ReactComponent domNode, String propName, Object propValue, Map<String, Object> previous)
   {
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

   public String toString()
   {
      return "<Mutate props: " + patch + ">";
   }

   // Hooks are not really implemented. See also widgets and thunks
   private static class Hook   
   {
      public HookListener hook;
      public void unHook(ReactComponent c, String name, Object value) {}
      public abstract static class HookListener
      {
         public abstract void somethingChanged(ReactComponent c, String name, Object previous);
      }
   }
   private static boolean isHook(Object anything)
   {
      return false;
   }

}
