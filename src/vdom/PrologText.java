package org.proactive.vdom;

import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;


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
