package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.WidgetContext;
import org.proactive.prolog.Engine;

public class Predicate_init_widget extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      // init_widget(Document, VNode, DomNode);
      try
      {
         //System.out.println("init_widget: " + args[1]);
         CompoundTerm element = (CompoundTerm)args[1];
         WidgetContext parentContext = (WidgetContext)((JavaObjectTerm)args[0]).value;
         WidgetContext widget = new WidgetContext(parentContext,
                                                  parentContext.getEngine(),
                                                  ((AtomTerm)element.args[0]).value,
                                                  element.args[1]);
         return interpreter.simpleUnify(args[2], new JavaObjectTerm(widget.init()));
      }
      catch (Exception e)
      {
         e.printStackTrace();
         return RC.FAIL;
      }
   }
}
