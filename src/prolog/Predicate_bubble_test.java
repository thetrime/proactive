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

public class Predicate_bubble_test extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Term handler = args[0];
      if (handler instanceof CompoundTerm && ((CompoundTerm)handler).tag == Engine.tagThis)
      {
         CompoundTerm handlerTerm = (CompoundTerm)handler;
         ReactWidget target = (ReactWidget)((JavaObjectTerm)(handlerTerm.args[0])).value;
         return target.triggerTest(handlerTerm.args[1], args[1]) ? RC.SUCCESS_LAST : RC.FAIL;
      }
      return RC.FAIL;
   }
}
