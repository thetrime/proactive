package org.proactive.prolog;

import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.interpreter.Predicate_call;
import gnu.prolog.vm.BacktrackInfo;

public class Predicate_colon extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactEnvironment environment = (ReactEnvironment)interpreter.getEnvironment();
      AtomTerm moduleName = (AtomTerm)args[0];
      RC rc;
      BacktrackInfo bi = null;
      if (backtrackMode)
      {
         bi = interpreter.popBacktrackInfo();
         bi.undo(interpreter);
      }
      try
      {
         environment.pushModule(moduleName.value);
         rc = Predicate_call.staticExecute(interpreter, backtrackMode, args[1]);
         if (rc == RC.SUCCESS)
         {
            // Make a fake backtrack point here so we get a chance to restore the module context if backtracking
            if (bi == null)
               bi = new BacktrackInfo(interpreter.getUndoPosition(), -1);
            interpreter.pushBacktrackInfo(bi);
         }
      }
      finally
      {
         environment.popModule();
      }
      return rc;
   }
}
