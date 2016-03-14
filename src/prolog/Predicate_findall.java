package org.proactive.prolog;

import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.Term;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.vm.BacktrackInfo;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.PrologCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.vm.interpreter.Predicate_call;
import java.util.LinkedList;
import java.util.Iterator;

public class Predicate_findall extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      int undo = interpreter.getUndoPosition();
      BacktrackInfo choicepoint = interpreter.peekBacktrackInfo();
      RC rc;
      LinkedList<Term> solutions = new LinkedList<Term>();
      try
      {
         rc = Predicate_call.staticExecute(interpreter, false, args[1]);
         while (rc != RC.FAIL)
         {
            solutions.add((Term)args[0].clone());
            if (rc == RC.SUCCESS_LAST)
               break;
            rc = Predicate_call.staticExecute(interpreter, true, args[1]);
         }
         if (rc == RC.SUCCESS_LAST)
            interpreter.undo(undo);
         Term listTerm = args[3];
         for (Iterator<Term> i = solutions.descendingIterator(); i.hasNext();)
            listTerm = CompoundTerm.getList(i.next(), listTerm);
         return interpreter.unify(args[2], listTerm);
      }
      catch (PrologException e)
      {
         interpreter.popBacktrackInfoUntil(choicepoint);
         interpreter.undo(undo);
         throw(e);
      }
   }
}
