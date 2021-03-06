package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactComponent;

public class Predicate_remove_child extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactComponent domNode = (ReactComponent)((JavaObjectTerm)args[0]).value;
      ReactComponent childNode = (ReactComponent)((JavaObjectTerm)args[1]).value;
      //System.out.println("Calling removeChild on" + domNode + " with " + childNode);
      domNode.removeChild(childNode);
      return RC.SUCCESS_LAST;
   }
}
