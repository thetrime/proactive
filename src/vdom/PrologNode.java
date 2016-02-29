package org.proactive.vdom;

import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;

public class PrologNode
{
   String nodeName;
   Map<String, Object> attributes = new HashMap<String, Object>();
   List<PrologNode> children = new LinkedList<PrologNode>();

   public PrologNode()
   {
   }
   
   // This is only really intended for use by the root component!
   public PrologNode(String nodeName)
   {
      this.nodeName = nodeName;
   }

   public String getKey()
   {
      return null;
   }
   
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
