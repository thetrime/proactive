package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.IntegerTerm;
import org.proactive.ReactWidget;
import javax.swing.SwingUtilities;
import java.awt.Window;
import java.awt.Component;

public class Predicate_media_size extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactEnvironment environment = (ReactEnvironment)interpreter.getEnvironment();
      ReactWidget widget = ((ReactWidget)environment.getCurrentContext(interpreter));
      Component c = widget.getAWTComponent();
      if (c != null)
      {
         Window w = SwingUtilities.getWindowAncestor(c);
         if (interpreter.simpleUnify(args[0], IntegerTerm.get(w.getWidth())) == RC.FAIL)
            return RC.FAIL;
         return interpreter.simpleUnify(args[1], IntegerTerm.get(w.getHeight()));
      }
      if (interpreter.simpleUnify(args[0], IntegerTerm.get(800)) == RC.FAIL)
         return RC.FAIL;
      return interpreter.simpleUnify(args[1], IntegerTerm.get(600));

   }
}
