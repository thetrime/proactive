package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.term.IntegerTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.AtomicTerm;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

public class Predicate_nb_setarg extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Term arg = args[0];
      Term term = args[1];
      Term value = args[2];
      if (term instanceof VariableTerm)
         PrologException.instantiationError(term);
      if (!(term instanceof CompoundTerm))
         PrologException.typeError(TermConstants.compoundAtom, term);
      if (!(value instanceof AtomicTerm))
         PrologException.typeError(TermConstants.atomicAtom, value);
      if (!(arg instanceof IntegerTerm))
         PrologException.typeError(TermConstants.integerAtom, arg);
      long i = ((IntegerTerm)arg).value;
      if (i < 0)
         PrologException.typeError(TermConstants.notLessThanZeroAtom, arg);

      // Voodoo time
      Term[] newArgs = new Term[((CompoundTerm)term).args.length];
      for (int j = 0; j < ((CompoundTerm)term).args.length; j++)
      {
         if (i == j)
            newArgs[j] = value;
         else
            newArgs[j] = ((CompoundTerm)term).args[j];
      }
      try
      {
         Field argsField = CompoundTerm.class.getField("args");
         argsField.setAccessible(true);
         argsField.set(term, newArgs);
      }
      catch(NoSuchFieldException nsfe)
      {
         return RC.FAIL;
      }
      catch(IllegalAccessException iae)
      {
         return RC.FAIL;
      }
      return RC.SUCCESS_LAST;
   }
}
