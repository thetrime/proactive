package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.AtomTerm;

public class Predicate_upcase_atom extends ExecuteOnlyCode
{
   private static int id = 0;
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      if (!(args[0] instanceof AtomTerm))
         PrologException.typeError(TermConstants.atomAtom, args[0]);
      return interpreter.unify(args[1], AtomTerm.get((((AtomTerm)args[0]).value).toUpperCase()));
   }
}
