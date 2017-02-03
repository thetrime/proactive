package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactComponent;
import org.proactive.ReactWidget;

public class Predicate_destroy_widget extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      // This is a no-op in the Swing client
      return RC.SUCCESS_LAST;
   }
}
