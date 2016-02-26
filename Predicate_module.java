import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.io.*;
import java.util.*;

public class Predicate_module extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactEnvironment environment = (ReactEnvironment)interpreter.getEnvironment();
      AtomTerm moduleName = (AtomTerm)args[0];
      List<CompoundTermTag> exports = new LinkedList<CompoundTermTag>();
      if (args[1] instanceof CompoundTerm)
      {
         CompoundTerm list = (CompoundTerm)args[1];
         if (list.tag == TermConstants.listTag)
         {
            while(list.tag.arity == 2)
            {
               if (list.args[0] instanceof CompoundTerm &&
                   ((CompoundTerm)list.args[0]).tag.arity == 2 &&
                   ((CompoundTerm)list.args[0]).tag.functor.value.equals("/"))
               {
                  CompoundTerm head = (CompoundTerm)list.args[0];
                  if (head.args[0] instanceof AtomTerm && head.args[1] instanceof IntegerTerm)
                     exports.add(CompoundTermTag.get(head));
                  else
                     PrologException.typeError(AtomTerm.get("predicate_indicator"), head);
                  
                  if (list.args[1] instanceof CompoundTerm)
                     list = (CompoundTerm)list.args[1];
                  else if (TermConstants.emptyListAtom.equals(list.args[1]))
                     break;
                  else
                     PrologException.typeError(AtomTerm.get("list"), args[1]);
               }
               else
                  PrologException.typeError(AtomTerm.get("predicate_indicator"), list.args[0]);
            }
         }
      }
      else if (!TermConstants.emptyListAtom.equals(args[1]))
         PrologException.typeError(AtomTerm.get("list"), args[1]);
      environment.startNewModule(moduleName.value, exports);
      return RC.SUCCESS_LAST;
   }
}
