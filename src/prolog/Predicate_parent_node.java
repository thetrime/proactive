package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.CompoundTermTag;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactComponent;

public class Predicate_parent_node extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactComponent domNode = (ReactComponent)((JavaObjectTerm)args[0]).value;
      if (domNode == null)
         return interpreter.simpleUnify(args[1], new CompoundTerm(CompoundTermTag.curly1, AtomTerm.get("null")));
      else
         return interpreter.simpleUnify(args[1], new JavaObjectTerm(domNode.getParentNode()));
   }
}
