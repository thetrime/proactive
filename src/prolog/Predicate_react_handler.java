package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactWidget;

public class Predicate_react_handler extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      // NB: This is actually called with 4 extra arguments, so really it is
      // react_handler(+TargetWidget, +ChainedHandler, +Event)
      // Chaining like this means we set NewState to [] - no change.
      // (Note that this means we do not actually have to re-render the component!

      System.out.println("react_handler(" + args[0] + ", " + args[1] + ", " + args[2] + ")");

      ReactWidget widget = (ReactWidget)((JavaObjectTerm)(args[0])).value;
      System.out.println("Triggering event on " + widget.getComponentName());
      widget.triggerEvent(args[1], args[2]);
      return RC.SUCCESS_LAST;
   }
}
