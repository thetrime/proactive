package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactComponent;

public class Predicate_replace_child extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactComponent domNode = (ReactComponent)((JavaObjectTerm)args[0]).value;
      ReactComponent newChild = (ReactComponent)((JavaObjectTerm)args[1]).value;
      ReactComponent oldChild = (ReactComponent)((JavaObjectTerm)args[2]).value;
      domNode.replaceChild(newChild, oldChild);
      return RC.SUCCESS_LAST;
   }
}
