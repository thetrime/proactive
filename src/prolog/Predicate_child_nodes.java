package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactComponent;
import java.util.List;
import java.util.LinkedList;
public class Predicate_child_nodes extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactComponent domNode = (ReactComponent)((JavaObjectTerm)args[0]).value;
      List<Term> list = new LinkedList<Term>();
      List<ReactComponent> children = domNode.getChildNodes();
      for (ReactComponent child : children)
         list.add(new JavaObjectTerm(child));
      return interpreter.simpleUnify(args[1], CompoundTerm.getList(list));
   }
}
