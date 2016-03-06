package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactComponent;

public class Predicate_create_text_node extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      PrologException.existenceError(AtomTerm.get("implementation"), AtomTerm.get("text_nodes"));
      return RC.FAIL;
   }
}
