package org.proactive.prolog;

import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.database.MetaPredicateInfo;
import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyMetaCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.interpreter.Predicate_call;
import gnu.prolog.vm.BacktrackInfoWithCleanup;
import gnu.prolog.vm.Environment;
import gnu.prolog.io.PrologStream;
import java.io.IOException;

public class Predicate_on_server extends ExecuteOnlyMetaCode
{
   public class ServerBacktrackInfo extends BacktrackInfoWithCleanup
   {
      protected int startUndoPosition;
      private Engine.ExecutionState state;
      private Term goal;

      protected ServerBacktrackInfo(Term goal)
      {
         super(null);
         this.goal = goal;
      }
      
      public RC nextSolution(Interpreter interpreter) throws PrologException
      {
         Engine.ExecutionState.RC rc = Engine.ExecutionState.RC.FAIL;
         try
         {
            rc = state.nextSolution();
         }
         catch (InterruptedException e)
         {
            throw new PrologException(AtomTerm.get("io_error"), e);
         }
         
         if (rc == Engine.ExecutionState.RC.FAIL)
            return RC.FAIL;
         if (rc == Engine.ExecutionState.RC.EXCEPTION)
            throw new PrologException(state.getException(), null);
         // Otherwise it succeeded, possibly with a choicepoint. Unify the result with args[0] and return the right value
         Term response = state.getResponse();
         // Since any PrologState objects in the goal will never unify with the response, replace them all with variables
         goal = deleteStates(goal);
         if (interpreter.simpleUnify(goal, response) == RC.FAIL)
         {
            // This is not good!
            return RC.FAIL;
         }
         if (rc == Engine.ExecutionState.RC.SUCCESS_LAST)
            return RC.SUCCESS_LAST;
         interpreter.pushBacktrackInfo(this);
         return RC.SUCCESS;
      }

      public void cleanup(Interpreter interpreter)
      {
         state.cut();
      }

      private Term deleteStates(Term t)
      {
         if (t instanceof PrologState)
            return new VariableTerm("State");
         else if (t instanceof CompoundTerm)
         {
            CompoundTerm c = (CompoundTerm)t;
            Term[] args = new Term[c.tag.arity];
            for (int i = 0; i < c.tag.arity; i++)
               args[i] = deleteStates(c.args[i]);
            return new CompoundTerm(c.tag, args);
         }
         return t;
      }
   }


   
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Environment environment = interpreter.getEnvironment();
      PrologStream stream;

      if (backtrackMode)
      {
         ServerBacktrackInfo bi = (ServerBacktrackInfo)interpreter.popBacktrackInfo();
         interpreter.undo(bi.startUndoPosition);
         return bi.nextSolution(interpreter);
      }
      else
      {
         ServerBacktrackInfo bi = new ServerBacktrackInfo(args[0]);
         bi.startUndoPosition = interpreter.getUndoPosition();
         try
         {
            bi.state = ((ReactEnvironment)environment).getEngine().prepareGoal(args[0], environment);
         }
         catch (IOException | InterruptedException e)
         {
            throw new PrologException(AtomTerm.get("io_error"), e);
         }
         return bi.nextSolution(interpreter);
      }
   }

	private static MetaPredicateInfo metaPredicateInfo = new MetaPredicateInfo(new MetaPredicateInfo.MetaType[]{MetaPredicateInfo.MetaType.META});
	public MetaPredicateInfo getMetaPredicateInfo()
	{
		return metaPredicateInfo;
	}


}

// FIXME: on cut, close the connection!
