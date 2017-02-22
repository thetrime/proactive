package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import org.proactive.prolog.Engine;

public class Predicate_broadcast_proactive_message extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Engine engine = ((ReactEnvironment)interpreter.getEnvironment()).getEngine();
      engine.sendAsyncMessage(args[0]);
      return RC.SUCCESS_LAST;
   }
}
