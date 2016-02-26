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
      System.out.println("Cross-module call to " + args[0] + " : " + args[1]);
      environment.pushModule(moduleName.value);
      System.out.println("Current module is " + environment.getModule());
      System.out.println("About to call " + args[1]);
      RC rc = Predicate_call.staticExecute(interpreter, false, args[1]);
      // Actually we need a backtrack point here to recover it...
      environment.popModule();
      return rc;
   }
}
