package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactWidget;
import org.proactive.prolog.Engine;

public class Predicate_init_widget extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      // init_widget(Document, VNode, DomNode);
      try
      {
         CompoundTerm element = (CompoundTerm)args[1];
         return interpreter.simpleUnify(args[2], new JavaObjectTerm(new ReactWidget((Engine)(((JavaObjectTerm)args[0]).value),
                                                                                    ((AtomTerm)element.args[0]).value,
                                                                                    element.args[1])));
      }
      catch (Exception e)
      {
         return RC.FAIL;
      }
   }
}
