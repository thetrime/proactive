package org.proactive.vdom;

import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.CompoundTerm;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;
import org.proactive.ReactComponentFactory;

public class PrologNode
{
   String nodeName;
   Map<String, Term> attributes = new HashMap<String, Term>();
   List<PrologNode> children = new LinkedList<PrologNode>();
   protected int count = 0;
   protected boolean hasWidgets = false;
   protected boolean hasThunks = false;
   public String getKey()
   {
      return null;
   }

   public int getCount()
   {
      return count;
   }

   public String getNodeName()
   {
      return nodeName;
   }

   public Map<String, Term> getAttributes()
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
      else if ((t instanceof CompoundTerm) &&
               ((CompoundTerm)t).tag.functor.value.equals("element") &&
               (((CompoundTerm)t).args[0] instanceof AtomTerm) &&
               ReactComponentFactory.isWidgetName(((AtomTerm)((CompoundTerm)t).args[0]).value))
         result = new PrologWidget(t, ((AtomTerm)((CompoundTerm)t).args[0]).value);
      else
         result = new PrologElement(t);
      return result;
   }

   public String toString()
   {
      if (getChildren() == null || getChildren().size() == 0)
         return "<" + getClass().getName() + ":" + nodeName + "/>";
      else
      {
         String tag = "<" + getClass().getName() + ":" + nodeName + ">";
         for (PrologNode child : getChildren())
            tag += child.toString();
         return tag + "</" + getClass().getName() + ":" + nodeName + ">";

      }
   }

   public boolean hasWidgets()
   {
      return hasWidgets;
   }

   public boolean hasThunks()
   {
      return hasThunks;
   }


}
