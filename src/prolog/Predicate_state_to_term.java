package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.JavaObjectTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.CompoundTermTag;
import org.proactive.ReactComponent;
import org.proactive.prolog.PrologState;

public class Predicate_state_to_term extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      // state_to_list(+State, -List)
      ReactEnvironment environment = (ReactEnvironment)interpreter.getEnvironment();
      if (PrologObject.isNull(args[0]))
         return interpreter.simpleUnify(args[2], PrologState.emptyState);

      if (!(args[0] instanceof PrologState))
	 PrologException.typeError(AtomTerm.get("state"), args[0]);
      PrologState state = (PrologState)args[0];
      return interpreter.simpleUnify(args[1], state.getTerm());
   }
}
