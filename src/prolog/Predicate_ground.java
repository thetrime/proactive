package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.term.CompoundTerm;
import java.util.Stack;

public class Predicate_ground extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Stack<Term> list = new Stack<Term>();
      list.push(args[0]);
      while (!list.empty())
      {
         Term t = list.pop().dereference();
         if (t instanceof VariableTerm)
            return RC.FAIL;
         else if (t instanceof CompoundTerm)
         {
            for (int i = 0; i < ((CompoundTerm)t).args.length; i++)
               list.push(((CompoundTerm)t).args[i]);
         }
      }
      return RC.SUCCESS_LAST;
   }
}
