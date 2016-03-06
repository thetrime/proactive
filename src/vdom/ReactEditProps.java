package org.proactive.vdom;

import org.proactive.ReactComponent;
import org.proactive.prolog.PrologObject;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import gnu.prolog.term.Term;

public class ReactEditProps extends ReactEdit
{
   Map<String,Term> patch;
   public ReactEditProps(PrologNode node, Map<String,Term> patch)
   {
      super(node);
      this.patch = patch;
   }

   public ReactComponent apply(ReactComponent domNode) throws Exception
   {
      Set<Map.Entry<String, Term>> entries = ((Map<String,Term>)patch).entrySet();
      Map<String,Term> previous = node.getAttributes();
      HashMap<String, PrologObject> newProperties = new HashMap<String, PrologObject>();
      for (Iterator<Map.Entry<String, Term>> i = entries.iterator(); i.hasNext();)
      {
         Map.Entry<String, Term> entry = i.next();
         String propName = entry.getKey();
         Term propValue = entry.getValue();
         if (propValue == null)
         {
            removeProperty(domNode, propName, propValue, previous, newProperties);
         }
         else if (isHook(propValue))
         {
            removeProperty(domNode, propName, propValue, previous, newProperties);
            Hook hook = Hook.get(propValue);
            if (hook != null)
               hook.somethingChanged(domNode, propName, previous!=null?previous.get(propName) : null);
         }
         else
         {
            // setProperty expects an object and sorts that out itself, so we dont need the isObject() case
            newProperties.put(propName, new PrologObject(propValue));
         }
      }
      System.out.println("Setting properties on " + domNode + " to " + newProperties);
      //domNode.setProperties(newProperties);
      return null;
   }

   private void removeProperty(ReactComponent domNode, String propName, Term propValue, Map<String, Term> previous, HashMap<String, PrologObject> newProperties)
   {
      if (previous != null)
      {
         Term previousValue = previous.get(propName);
         if (!isHook(previousValue))
         {
            if (propName.equals("attributes"))
            {
               // FIXME: Stub?
            }
            else if (propName.equals("style"))
            {
               // FIXME: Stub?
            }
            else
               newProperties.put(propName, null);
         }
         else
            Hook.get(previousValue).unHook(domNode, propName, propValue);
      }
   }

   public String toString()
   {
      return "<Mutate props: " + patch + ">";
   }

   // Hooks are not really implemented. See also widgets and thunks
   private static class Hook   
   {
      public static Hook get(Term t) { return null; }
      public void unHook(ReactComponent c, String name, Term value) {}
      public void somethingChanged(ReactComponent c, String name, Term previous) {}
   }
   private static boolean isHook(Term anything)
   {
      return false;
   }

}
