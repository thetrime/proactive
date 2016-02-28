package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;

public class Predicate_raise_event extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {      
      FluxDispatcher.queueEvent(args[0], args[1]);
      FluxDispatcher.dispatchEvents();
      return RC.SUCCESS_LAST;
   }
}
