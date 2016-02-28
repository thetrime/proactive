package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.AtomTerm;
import java.util.List;
import java.util.LinkedList;
import gnu.prolog.vm.TermConstants;

public class Predicate_wait_for extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      if (TermConstants.emptyListAtom.equals(args[0]))
         return RC.SUCCESS_LAST;
      List<String> tokens = new LinkedList<String>();      
      if (args[0] instanceof CompoundTerm)
      {
         CompoundTerm list = (CompoundTerm)args[0];
         while (list.tag.arity == 2 && list.tag.functor.value.equals("."))
         {
            if (list.args[0] instanceof AtomTerm)
               tokens.add(((AtomTerm)list.args[0]).value);
            else
               PrologException.typeError(AtomTerm.get("atom"), list.args[0]);
            if (list.args[1] instanceof CompoundTerm)
               list = (CompoundTerm)list.args[1];
            else if (TermConstants.emptyListAtom.equals(list.args[1]))
               break;
            else
               PrologException.typeError(AtomTerm.get("list"), args[0]);
         }
      }      
      FluxDispatcher.waitFor(tokens);
      return RC.SUCCESS_LAST;
   }
}
