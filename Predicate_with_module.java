import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.io.*;
import java.util.*;
import gnu.prolog.vm.interpreter.*;


public class Predicate_with_module extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactEnvironment environment = (ReactEnvironment)interpreter.getEnvironment();
      AtomTerm moduleName = (AtomTerm)args[0];
      environment.pushModule(moduleName.value);
      System.out.println("Pushed " + moduleName + " and will now call " +args[1]);
      System.exit(-1);
      RC rc = Predicate_call.staticExecute(interpreter, false, args[1]);
      // Actually we need a backtrack point here to recover it...
      environment.popModule();
      return rc;
   }
}


// Do NOT import with_module from user into Splunge. That will just define Splunge:with_module(A,B) :- with_module(A,B) and we get a loop
// Instead, for every module we create, we must define with_module/2 separately
