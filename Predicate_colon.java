import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.io.*;
import java.util.*;
import gnu.prolog.vm.interpreter.*;

public class Predicate_colon extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactEnvironment environment = (ReactEnvironment)interpreter.getEnvironment();
      AtomTerm moduleName = (AtomTerm)args[0];
      RC rc;
      BacktrackInfo bi = null;
      if (backtrackMode)
      {
         bi = interpreter.popBacktrackInfo();
         bi.undo(interpreter);
      }
      try
      {
         environment.pushModule(moduleName.value);
         rc = Predicate_call.staticExecute(interpreter, backtrackMode, args[1]);
         if (rc == RC.SUCCESS)
         {
            // Make a fake backtrack point here so we get a chance to restore the module context if backtracking
            if (bi == null)
               bi = new BacktrackInfo(interpreter.getUndoPosition(), -1);
            interpreter.pushBacktrackInfo(bi);
         }
      }
      finally
      {
         environment.popModule();
      }
      return rc;
   }
}
