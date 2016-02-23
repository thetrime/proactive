import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.io.*;
import java.util.*;

public class Predicate_java_println extends ExecuteOnlyCode
{
   private static int id = 0;
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Environment environment = interpreter.getEnvironment();
      PrologStream stream;
      if (!(args[0] instanceof AtomTerm))
         PrologException.typeError(TermConstants.atomAtom, args[0]);
      System.out.println(((AtomTerm)args[0]).value);
      return RC.SUCCESS_LAST;
   }
}
