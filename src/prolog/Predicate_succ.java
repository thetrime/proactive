package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.IntegerTerm;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.AtomTerm;

public class Predicate_succ extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      if (args[0] instanceof VariableTerm)
      {
         if (!(args[1] instanceof IntegerTerm))
            PrologException.typeError(TermConstants.integerAtom, args[1]);
         IntegerTerm i = (IntegerTerm)args[1];
         if (i.value == 0)
            return RC.FAIL;
         return interpreter.unify(args[0], IntegerTerm.get(i.value-1));
      }
      if (!(args[1] instanceof VariableTerm) && !(args[1] instanceof IntegerTerm))
         PrologException.typeError(TermConstants.integerAtom, args[1]);
      if (!(args[0] instanceof IntegerTerm))
         PrologException.typeError(TermConstants.integerAtom, args[0]);
      return interpreter.unify(args[1], IntegerTerm.get(((IntegerTerm)args[0]).value + 1));
   }
}
