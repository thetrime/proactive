package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactWidget;
import org.proactive.ReactComponent;
import org.proactive.prolog.Engine;

public class Predicate_update_widget extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      // update_widget(+NewVDom, +OldVDom, +DomNode, -NewNode);
      try
      {
         CompoundTerm newvDom = (CompoundTerm)args[0];
         CompoundTerm oldvDom = (CompoundTerm)args[1];
         ReactComponent domNode = (ReactComponent)((JavaObjectTerm)args[2]).value;
         ReactWidget widget = ((ReactWidget)domNode);
         System.out.println("update_widget(" + args[0] + ", " + args[1] + ", " + args[2] + ", _)");
         widget.updateWidget(newvDom.args[1]);
         return interpreter.simpleUnify(args[3], new JavaObjectTerm(domNode));
      }
      catch (Exception e)
      {
         e.printStackTrace();
         return RC.FAIL;
      }
   }
}
