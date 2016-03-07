package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.WidgetContext;
import org.proactive.ReactComponent;
import org.proactive.prolog.Engine;

public class Predicate_update_widget extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      // update_widget(+NewVDom, +OldVDom, +DomNode, -NewNode);
      try
      {
         CompoundTerm widget = (CompoundTerm)args[0];
         CompoundTerm vnode = (CompoundTerm)args[1];
         ReactComponent domNode = (ReactComponent)((JavaObjectTerm)args[2]).value;
         System.out.println("update_widget(" + args[0] + ", " + args[1] + ", " + args[2] + ", _)");
         domNode.getOwnerDocument().updateWidget(widget, domNode);
         return interpreter.simpleUnify(args[3], new JavaObjectTerm(domNode));
      }
      catch (Exception e)
      {
         e.printStackTrace();
         return RC.FAIL;
      }
   }
}
