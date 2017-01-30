package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.AtomTerm;

public class Predicate_memberchk extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      int undoPosition = interpreter.getUndoPosition();
      Term list = args[1];
      while(CompoundTerm.isListPair(list))
      {
         Term head = ((CompoundTerm) list).args[0].dereference();
         if (interpreter.unify(head, args[0]) == RC.FAIL)
         {
            interpreter.undo(undoPosition);
            list = ((CompoundTerm) list).args[1].dereference();
            continue;
         }
         else
            return RC.SUCCESS;
      }
      if (list instanceof VariableTerm)
         return interpreter.unify(args[0], CompoundTerm.getList(args[1], CompoundTerm.getList(args[1], new VariableTerm())));
      return RC.FAIL;
   }
}
