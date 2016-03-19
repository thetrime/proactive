package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactComponent;

public class Predicate_get_store_state extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactEnvironment environment = (ReactEnvironment)interpreter.getEnvironment();
      Engine engine = environment.getEngine();
      Term t = engine.getStoreState(((AtomTerm)args[0]).value);
      return interpreter.simpleUnify(args[1], t);
   }
}
