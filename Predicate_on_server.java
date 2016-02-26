import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.io.*;
import java.util.*;

public class Predicate_on_server extends ExecuteOnlyCode
{
   public class ServerBacktrackInfo extends BacktrackInfo
   {
      protected int startUndoPosition;
      private Engine.ExecutionState state;
      private Term goal;

      protected ServerBacktrackInfo(Term goal)
      {
         super(-1, -1);
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
         interpreter.simpleUnify(goal, state.getResponse());
         if (rc == Engine.ExecutionState.RC.SUCCESS_LAST)
            return RC.SUCCESS_LAST;
         interpreter.pushBacktrackInfo(this);
         return RC.SUCCESS;
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
            bi.state = Engine.prepareGoal(args[0], environment);
         }
         catch (IOException e )
         {
            throw new PrologException(AtomTerm.get("io_error"), e);
         }
         return bi.nextSolution(interpreter);
      }
         
   }
}

// FIXME: on cut, close the connection!
