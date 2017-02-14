package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.AtomTerm;

public class Predicate_callable extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      if (args[0] instanceof AtomTerm || args[0] instanceof CompoundTerm)
         return RC.SUCCESS_LAST;
      return RC.FAIL;
   }
}
