package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.IntegerTerm;

public class Predicate_nth0 extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      int index = ((IntegerTerm)args[0]).value;
      Term list = args[1];
      int i;
      for (i = 0; i < index && CompoundTerm.isListPair(list); i++)
         list = ((CompoundTerm)list).args[1].dereference();
      if (i == index && CompoundTerm.isListPair(list))
         return interpreter.unify(args[2], ((CompoundTerm)list).args[0].dereference());
      return RC.FAIL;
   }
}
