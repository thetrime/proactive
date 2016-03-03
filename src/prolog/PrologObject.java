package org.proactive.prolog;

import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.IntegerTerm;
import gnu.prolog.term.CompoundTerm;

// This is a wrapper for Term with a number of methods for getting the data out in a standardized way
// Intended for use by the ui classes so they dont have to worry about the details of Term
public class PrologObject
{
   Term term;
   public PrologObject(Term term)
   {
      this.term = term;
   }

   public String asString()
   {
      Term t = Engine.unpack(term);
      if (t instanceof AtomTerm)
         return ((AtomTerm)t).value;
      return t.toString();
   }

   public int asFill()
   {
      Term t = Engine.unpack(term);
      if (t instanceof AtomTerm)
      {
         String fill = ((AtomTerm)term).value;
         if (fill.equals("horizontal"))
            return java.awt.GridBagConstraints.HORIZONTAL;
         else if (fill.equals("vertical"))
            return java.awt.GridBagConstraints.VERTICAL;
         else if (fill.equals("both"))
            return java.awt.GridBagConstraints.BOTH;
      }
      return java.awt.GridBagConstraints.NONE;      
   }

   public String asOrientation()
   {
      Term t = Engine.unpack(term);
      if (t instanceof AtomTerm)
      {
         String orientation = ((AtomTerm)term).value;
         return orientation;
         /*
         if (orientation.equals("horizontal"))
            return ;
         else if (orientation.equals("vertical"))
            return 1;
         else
            System.out.println("Illegal orientation: " + t);
         */
      }
      return "vertical";
   }

   public int asInteger()
   {
      Term t = Engine.unpack(term);
      if (t instanceof IntegerTerm)
         return ((IntegerTerm)t).value;
      else if (t instanceof AtomTerm)
         return Integer.parseInt(((AtomTerm)t).value);
      return 0;
   }

   public String asScroll()
   {
      Term t = Engine.unpack(term);
      if (t instanceof AtomTerm)
         return((AtomTerm)term).value;
      return "vertical";
   }
   public Term asTerm()
   {
      return term;
   }

}
