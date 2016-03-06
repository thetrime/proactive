package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactComponent;

public class Predicate_destroy_widget extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactComponent domNode = (ReactComponent)((JavaObjectTerm)args[0]).value;
      // FIXME: implement
      //Widget widget = null;
      //widget.destroy(domNode);
      return RC.SUCCESS_LAST;
   }
}
