package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.JavaObjectTerm;
import gnu.prolog.vm.TermConstants;
import org.proactive.ReactComponent;
import org.proactive.prolog.Engine;
import java.util.HashMap;

public class Predicate_set_vdom_properties extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactComponent domNode = (ReactComponent)((JavaObjectTerm)args[0]).value;
      domNode.setProperties(Engine.termToProperties(args[1]));
      return RC.SUCCESS_LAST;
   }
}
