package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.AtomicTerm;
import gnu.prolog.term.FloatTerm;
import gnu.prolog.term.IntegerTerm;

public class Predicate_atomic_list_concat extends ExecuteOnlyCode
{
   private static int id = 0;
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Term atomics = args[0].dereference();
      Term sep = args[1].dereference();
      Term out = args[2].dereference();
      if (atomics instanceof VariableTerm)
         PrologException.instantiationError(atomics);
      if (!(sep instanceof AtomicTerm))
         PrologException.typeError(TermConstants.atomAtom, sep);
      if (atomics == TermConstants.emptyListAtom)
         return interpreter.unify(out, AtomTerm.get(""));

      StringBuilder sb = new StringBuilder();
      Term list = atomics.dereference();
      System.out.println("list: " + list);
      while (list instanceof CompoundTerm && ((CompoundTerm)list).tag == TermConstants.listTag)
      {
         System.out.println("Is a list: " + list);
         Term head = ((CompoundTerm)list).args[0].dereference();
         list = ((CompoundTerm)list).args[1].dereference();
         if (head instanceof AtomTerm)
            sb.append(((AtomTerm)head).value);
         else if (head instanceof IntegerTerm)
            sb.append(((IntegerTerm)head).value);
         else if (head instanceof FloatTerm)
            sb.append(((FloatTerm)head).value);
         else
            PrologException.typeError(TermConstants.atomicAtom, head);
         if (list != TermConstants.emptyListAtom)
            sb.append(((AtomTerm)sep).value);
      }
      if (list != TermConstants.emptyListAtom)
         PrologException.typeError(TermConstants.listAtom, atomics);
      return interpreter.unify(AtomTerm.get(sb.toString()), args[2]);
   }
}
