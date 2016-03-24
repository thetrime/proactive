package org.proactive.prolog;

import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.CompoundTermTag;
import gnu.prolog.term.JavaObjectTerm;
import gnu.prolog.term.AtomicTerm;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.vm.PrologException;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.LinkedList;

public class PrologState extends AtomicTerm
{
   private HashMap<AtomTerm, Term> map = null;
   public static final PrologState emptyState = new PrologState();
   public static final Term nullTerm = new CompoundTerm(CompoundTermTag.curly1, AtomTerm.get("null"));

   @Override
   public int getTermType()
   {
      return JAVA_OBJECT;
   }

   private PrologState()
   {
      map = new HashMap<AtomTerm, Term>();
   }

   private PrologState(HashMap<AtomTerm, Term> map)
   {
      this.map = map;
   }
   public PrologState(Term t) throws PrologException
   {
      map = new HashMap<AtomTerm, Term>();
      processElements(t);
   }

   public static PrologState fromList(Term t) throws PrologException
   {
      // This is used for parsing the attributes list into a state
      PrologState prologState = new PrologState();
      while ((t = t.dereference()) instanceof CompoundTerm)
      {
	 CompoundTerm ct = (CompoundTerm) t;
	 if (ct.tag != TermConstants.listTag)
	    PrologException.typeError(TermConstants.listAtom, t);
	 else
	    prologState.processElement(ct.args[0], CompoundTermTag.get("=", 2));
	 t = ct.args[1];
      }
      return prologState;
   }

   private void processElements(Term t) throws PrologException
   {
      if (t instanceof VariableTerm)
	 return; // empty
      if (TermConstants.emptyCurlyAtom.equals(t))
	 return; // empty
      if (t instanceof CompoundTerm && ((CompoundTerm)t).tag == CompoundTermTag.curly1)
      {
	 // This is a bit like list processing except that rather than ./2 we have ,/2, and there is no tail
	 Term list = ((CompoundTerm)t).args[0];
	 while (list instanceof CompoundTerm && ((CompoundTerm)list).tag == CompoundTermTag.comma)
	 {
	    Term head = ((CompoundTerm)list).args[0];
	    processElement(head, CompoundTermTag.get(":", 2));
	    list = ((CompoundTerm)list).args[1];
	 }
	 processElement(list, CompoundTermTag.get(":", 2));
      }
      else
	 PrologException.typeError(AtomTerm.get("state"), t);
   }

   private void processElement(Term t, CompoundTermTag functor) throws PrologException
   {
      if (t instanceof CompoundTerm && ((CompoundTerm)t).tag == functor)
      {
	 Term key = ((CompoundTerm)t).args[0];
	 Term value = ((CompoundTerm)t).args[1];
	 if (!(key instanceof AtomTerm))
	    PrologException.typeError(AtomTerm.get("atom"), key);
	 processElement((AtomTerm)key, value, functor);
      }
      else
	 PrologException.typeError(AtomTerm.get("state_element"), t);
   }

   public static boolean isState(Term t)
   {
      return (t instanceof CompoundTerm && ((CompoundTerm)t).tag == CompoundTermTag.curly1 && AtomTerm.get("null") != ((CompoundTerm)t).args[0]);
   }

   private void processElement(AtomTerm key, Term value, CompoundTermTag functor) throws PrologException
   {
      Term existingValue = map.get(key);
      if (existingValue == null)
      {
	 if (isState(value))
	    map.put((AtomTerm)key, new PrologState(value));
	 else
	    map.put((AtomTerm)key, (Term)value.clone());
      }
      else
      {
	 if (existingValue instanceof PrologState)
	 {
	    PrologState existingState = (PrologState)existingValue;
	    if (isState(value))
	    {
	       // Merging objects
	       PrologState newState = existingState.cloneWith(value);
	       map.put(key, newState);
	    }
	    else
	    {
	       // Changing {foo: {bar: .....}} -> {foo: atomic-type}
	       map.put(key, (Term)value.clone());
	    }
	 }
	 else
	 {
	    if (isState(value))
	       map.put((AtomTerm)key, new PrologState(value));
	    else
	       map.put((AtomTerm)key, (Term)value.clone());
	 }
      }
   }

   public PrologState cloneWith(Term t) throws PrologException
   {
      HashMap<AtomTerm, Term> newMap = new HashMap<AtomTerm, Term>();
      for (Map.Entry<AtomTerm, Term> entry : map.entrySet())
	 newMap.put(entry.getKey(), entry.getValue());
      PrologState result = new PrologState(newMap);
      if (t instanceof PrologState)
      {
	 for (Map.Entry<AtomTerm, Term> entry : ((PrologState)t).map.entrySet())
	 {
	    // FIXME: Should this be :/2?
	    result.processElement(entry.getKey(), entry.getValue(), CompoundTermTag.get(":", 2));
	 }
      }
      else
	 result.processElements(t);
      return result;
   }

   public String toString()
   {
      return map.toString();
   }

   public void put(AtomTerm key, Term value)
   {
      map.put(key, value);
   }

   public Term get(AtomTerm key)
   {
      Term value = map.get(key);
      if (value == null)
	 return nullTerm;
      return value;
   }


   public HashMap<String, PrologObject> getProperties()
   {
      HashMap<String, PrologObject> result = new HashMap<String, PrologObject>();
      for (Map.Entry<AtomTerm, Term> entry : map.entrySet())
	 result.put(entry.getKey().value, new PrologObject(entry.getValue()));
      return result;
   }

}
