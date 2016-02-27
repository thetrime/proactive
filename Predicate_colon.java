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
      
      environment.pushModule(moduleName.value);
      RC rc = Predicate_call.staticExecute(interpreter, false, args[1]);
      // FIXME: Actually we need a backtrack point here to reset the module if rc is SUCCESS
      environment.popModule();
      return rc;
   }
}
