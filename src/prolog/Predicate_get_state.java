package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.JavaObjectTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.CompoundTermTag;
import org.proactive.ReactComponent;
import org.proactive.prolog.PrologState;

public class Predicate_get_state extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      // get_state(+State, +Key, -Value)
      // If Value is missing, bind it to {null}
      // If State is {null}, bind Value to {null} (this allows chaining like foo.bar.baz in case foo.bar is not defined)
      ReactEnvironment environment = (ReactEnvironment)interpreter.getEnvironment();
      if (PrologObject.isNull(args[0]))
	 return interpreter.simpleUnify(args[2], PrologState.nullTerm);

      if (!(args[0] instanceof PrologState))
	 PrologException.typeError(AtomTerm.get("state"), args[0]);
      PrologState state = (PrologState)args[0];
      if (args[1] instanceof AtomTerm)
	 return interpreter.simpleUnify(args[2], state.get((AtomTerm)args[1]));
      if (args[1] instanceof CompoundTerm)
      {
	 // Otherwise we must glue args
	 CompoundTerm term = (CompoundTerm)args[1];
	 Term[] glueArgs = term.args;
	 Term result = state.get(term.tag.functor);
	 if (PrologObject.isNull(result))
	    return interpreter.simpleUnify(args[2], result);
	 if (result instanceof CompoundTerm)
	 {
	    CompoundTerm ct = (CompoundTerm)result;
	    if (ct.tag == CompoundTermTag.get("$this", 2))
	    {
	       if (ct.args[1] instanceof CompoundTerm && ((CompoundTerm)ct.args[1]).tag == CompoundTermTag.get(":",2))
	       {
		  Term module = ((CompoundTerm)ct.args[1]).args[0];
		  Term goal = ((CompoundTerm)ct.args[1]).args[1];
		  Term newGoal = addArgs(goal, glueArgs);
		  return interpreter.simpleUnify(args[2], new CompoundTerm(ct.tag, new Term[]{ct.args[0], new CompoundTerm(CompoundTermTag.get(":", 2), module, newGoal)}));
	       }
	       else
	       {
		  // No module
		  Term newGoal = addArgs(ct.args[1], glueArgs);
		  return interpreter.simpleUnify(args[2], new CompoundTerm(ct.tag, new Term[]{ct.args[0], newGoal}));
	       }
	    }
	    PrologException.typeError(AtomTerm.get("gluable"), ct);
	 }
      }
      PrologException.typeError(AtomTerm.get("key"), args[1]);
      return null;
   }


   private Term addArgs(Term goal, Term[] glueArgs) throws PrologException
   {
      if (goal instanceof AtomTerm)
      {
	 return new CompoundTerm((AtomTerm)goal, glueArgs);
      }
      else if (goal instanceof CompoundTerm)
      {
	 CompoundTerm cgoal = (CompoundTerm)goal;
	 Term[] newArgs = new Term[glueArgs.length + cgoal.args.length];
	 int i;
	 for (i = 0; i < cgoal.args.length; i++)
	    newArgs[i] = cgoal.args[i];
	 for (int j = 0; j < glueArgs.length; j++)
	    newArgs[i++] = glueArgs[j];
	 return new CompoundTerm(((CompoundTerm)goal).tag.functor, newArgs);
      }
      else
	 PrologException.typeError(AtomTerm.get("callable"), goal);
      return null;
   }

}
