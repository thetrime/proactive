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
import org.proactive.prolog.Engine;
import gnu.prolog.vm.interpreter.Predicate_call;

public class Predicate_bubble_event extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Term handler = args[0];
      if (handler instanceof CompoundTerm && ((CompoundTerm)handler).tag == Engine.tagThis)
      {
         CompoundTerm handlerTerm = (CompoundTerm)handler;
         ReactWidget target = (ReactWidget)((JavaObjectTerm)(handlerTerm.args[0])).value;
         //System.out.println("Bubbling trigger to " + target + ": " + handlerTerm.args[1]);
         if (target.triggerEvent(handlerTerm.args[1], args[1]))
            return RC.SUCCESS_LAST;
         return RC.FAIL;
      }
      // Otherwise it is just a goal - go ahead and call it with one extra arg
      Term goal = null;
      if (handler instanceof AtomTerm)
      {
         goal = new CompoundTerm((AtomTerm)handler, new Term[]{args[1]});
      }
      else if (handler instanceof CompoundTerm)
      {
         CompoundTerm handlerTerm = (CompoundTerm)handler;
         Term[] newArgs = new Term[handlerTerm.args.length+1];
         for (int i = 0; i < handlerTerm.args.length; i++)
            newArgs[i] = handlerTerm.args[i];
	 newArgs[handlerTerm.args.length] = args[1];
	 goal = new CompoundTerm(handlerTerm.tag, newArgs);
      }
      else
      {
         PrologException.typeError(AtomTerm.get("callable"), handler);
         return null; // Not really reachable
      }
      //System.out.println("calling goal directly: " + goal);
      return Predicate_call.staticExecute(interpreter, backtrackMode, goal);
   }
}
