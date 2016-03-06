package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactComponent;
import org.proactive.ReactComponentFactory;

public class Predicate_create_element extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      // ignore document (args[0]) for now. Really this is ReactComponentFactory
      try
      {
         return interpreter.simpleUnify(args[2], new JavaObjectTerm(ReactComponentFactory.createElement(((AtomTerm)args[1]).value)));
      }
      catch (Exception e)
      {
         return RC.FAIL;
      }
   }
}
