package org.proactive.prolog;

import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.IntegerTerm;
import gnu.prolog.term.FloatTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.CompoundTermTag;
import gnu.prolog.vm.TermConstants;
import org.proactive.ui.ProactiveConstraints;
import java.util.Map;
import java.util.List;
import java.util.LinkedList;


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

   public boolean isNull()
   {
      return isNull(term);
   }

   public String asString()
   {
      Term t = term; //Engine.unpack(term);
      if (t instanceof AtomTerm)
         return ((AtomTerm)t).value;
      if (t instanceof IntegerTerm)
        return String.valueOf(((IntegerTerm)t).value);
      if (t instanceof FloatTerm)
	return String.valueOf(((FloatTerm)t).value);
      else if (isNull())
         return null;
         System.out.println("Warning: asString called on " + term + " of type " + term.getClass() + " which unpacks something which is not a string: " + t + " of type " + t.getClass());
      return t.toString();
   }

   public ProactiveConstraints.Fill asFill()
   {
      Term t = term; //Engine.unpack(term);
      if (t instanceof AtomTerm)
      {
         String fill = ((AtomTerm)term).value;
         if (fill.equals("horizontal"))
            return ProactiveConstraints.Fill.HORIZONTAL;
         else if (fill.equals("vertical"))
            return ProactiveConstraints.Fill.VERTICAL;
         else if (fill.equals("both"))
            return ProactiveConstraints.Fill.BOTH;
      }
      return ProactiveConstraints.Fill.NONE;
   }

   public String asOrientation()
   {
      Term t = term;
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
      Term t = term;
      if (t instanceof IntegerTerm)
         return ((IntegerTerm)t).value;
      else if (t instanceof AtomTerm)
         return Integer.parseInt(((AtomTerm)t).value);
      return 0;
   }

   public boolean asBoolean()
   {
      Term t = term;
      if (t instanceof AtomTerm)
         return ((AtomTerm)t).value.equals("true");
      return false;
   }


   public String asScroll()
   {
      Term t = term;
      if (t instanceof AtomTerm)
         return((AtomTerm)term).value;
      return "vertical";
   }
   public Term asTerm()
   {
      return term;
   }

   public List<PrologObject> asList()
   {
      if (isNull())
         return null;
      List<PrologObject> list = new LinkedList<PrologObject>();
      if (TermConstants.emptyListAtom.equals(term))
         return list;
      else if (term instanceof CompoundTerm)
      {
         CompoundTerm c = (CompoundTerm)term;
         while(c.tag.arity == 2)
         {
            list.add(new PrologObject(c.args[0]));
            if (c.args[1] instanceof CompoundTerm)
               c = (CompoundTerm)c.args[1];
            else if (TermConstants.emptyListAtom.equals(c.args[1]))
               break;
            else
               return null;
         }
         return list;
      }
      else
         return null;
   }

   public class NameValuePair
   {
      private String key;
      private PrologObject value;
      public NameValuePair(String key, PrologObject value)
      {
         this.key = key;
         this.value = value;
      }
      public String getKey()
      {
         return key;
      }
      public PrologObject getValue()
      {
         return value;
      }

   }

   public NameValuePair asNameValuePair()
   {
      if (term instanceof CompoundTerm)
      {
         CompoundTerm c = (CompoundTerm)term;
         if (c.tag.arity == 2 && c.tag.functor.value.equals("="))
         {
            if (c.args[0] instanceof AtomTerm)
               return new NameValuePair(((AtomTerm)c.args[0]).value, new PrologObject(c.args[1]));
         }
      }
      return null;
   }

   public static PrologObject emptyList()
   {
      return new PrologObject(TermConstants.emptyListAtom);
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
      if (value == null)
         return new CompoundTerm(CompoundTermTag.curly1, AtomTerm.get("null"));
      if (value instanceof String)
         return AtomTerm.get((String)value);
      if (value instanceof Boolean)
         return (((Boolean)value).booleanValue())?AtomTerm.get("true"):AtomTerm.get("false");
      if (value instanceof PrologObject)
         return ((PrologObject)value).asTerm();
      if (value instanceof Integer)
         return IntegerTerm.get((Integer)value);
      return new CompoundTerm(CompoundTermTag.curly1, AtomTerm.get("null"));
   }

   public String toString()
   {
      return "PrologObject(" + term + ")";
   }

}
