import gnu.prolog.term.Term;
import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;

public class Predicate_java_println extends ExecuteOnlyCode
{
   private static int id = 0;
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      System.out.println(args[0]);
      return RC.SUCCESS_LAST;
   }
}
