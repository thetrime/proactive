package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.AtomTerm;

public class Predicate_memberchk extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      rc = Predicate_call.staticExecute(interpreter, false, args[1]);
      BacktrackInfo bi = interpreter.getUndoPosition();
      while(CompoundTerm.isListPair(list))
      {
         Term head = ((CompoundTerm) list).args[0].dereference();
         if (interpreter.unify(head, args[0]))
            return RC.SUCCESS;
         interpreter.undo(bi);
         list = ((CompoundTerm) bi.list).args[1].dereference();
      }
      if (list instanceof VariableTerm)
      {
         if (interpreter.unify(args[0], CompoundTerm.getList(args[1], CompoundTerm.getList(args[1], new VariableTerm()))))
            return RC.SUCCESS;
      }
      return RC.FAIL;
   }
}
