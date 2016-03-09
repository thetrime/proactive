package org.proactive.prolog;

import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.IntegerTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.CompoundTermTag;
import gnu.prolog.vm.TermConstants;
import java.util.Map;


// This is a wrapper for Term with a number of methods for getting the data out in a standardized way
// Intended for use by the ui classes so they dont have to worry about the details of Term
public class PrologObject
{
   Term term;
   public PrologObject(Term term)
   {
      this.term = term; // Engine.unpack(term, null);
   }

   public static boolean isNull(Term t)
   {
      if (t instanceof CompoundTerm)
      {
         CompoundTerm c = (CompoundTerm)t;
         return (c.tag == CompoundTermTag.curly1 && c.args[0] instanceof AtomTerm && "null".equals(((AtomTerm)c.args[0]).value));
      }
      return false;

   }

   public String asString()
   {
      Term t = term; //Engine.unpack(term);
      if (t instanceof AtomTerm)
         return ((AtomTerm)t).value;
      else if (isNull(t))
         return null;
         System.out.println("Warning: asString called on " + term + " of type " + term.getClass() + " which unpacks something which is not a string: " + t + " of type " + t.getClass());
      return t.toString();
   }

   public int asFill()
   {
      Term t = term; //Engine.unpack(term);
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
      Term t = term; //Engine.unpack(term);
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
      Term t = term; //Engine.unpack(term);
      if (t instanceof IntegerTerm)
         return ((IntegerTerm)t).value;
      else if (t instanceof AtomTerm)
         return Integer.parseInt(((AtomTerm)t).value);
      return 0;
   }

   public boolean asBoolean()
   {
      Term t = term; //Engine.unpack(term);
      if (t instanceof AtomTerm)
         return ((AtomTerm)t).value.equals("true");
      return false;
   }


   public String asScroll()
   {
      Term t = term; //Engine.unpack(term);
      if (t instanceof AtomTerm)
         return((AtomTerm)term).value;
      return "vertical";
   }
   public Term asTerm()
   {
      return term;
   }



   public static PrologObject serialize(Map<String, Object> map)
   {
      return new PrologObject(serializeMap(map));
   }

   private static Term serializeMap(Map<String, Object> map)
   {
      if (map.size() == 0)
         return TermConstants.emptyListAtom;
      Term[] elements = new Term[map.size()];
      int i = 0;
      for (Map.Entry<String, Object> entry : map.entrySet())
      {
         Term[] args = new Term[2];
         args[0] = AtomTerm.get(entry.getKey());
         args[1] = serializeObject(entry.getValue());
         CompoundTerm pair = new CompoundTerm("=", args);
         elements[i++] = pair;
      }
      return CompoundTerm.getList(elements);
   }

   public static Term serializeObject(Object value)
   {
      if (value instanceof String)
         return AtomTerm.get((String)value);
      return new CompoundTerm(CompoundTermTag.curly1, AtomTerm.get("null"));
   }

   public String toString()
   {
      return "PrologObject(" + term + ")";
   }

}
