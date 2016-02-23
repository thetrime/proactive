import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;
import java.io.*;
import java.net.*;
import java.lang.reflect.*;

public class Engine
{
   private Environment env;
   private Interpreter interpreter;
   public Engine() throws Exception
   {
      env = new Environment();
      env.ensureLoaded(AtomTerm.get("sample.pl"));
      interpreter = env.createInterpreter();
   }
   
   public PrologDocument render(String component, Term state, Term props) throws Exception
   {
      System.out.println("Rendering " + component);
      if (state == null)
         state = TermConstants.emptyListAtom;
      if (props == null)
         props = TermConstants.emptyListAtom;
      VariableTerm replyTerm = new VariableTerm("Result");
      Term goal = new CompoundTerm(AtomTerm.get("render_" + component), new Term[]{state, props, replyTerm});
      interpreter.undo(0);
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      PrologCode.RC rc = interpreter.execute(g);
      if (rc == PrologCode.RC.SUCCESS)
         interpreter.stop(g);
      if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
      {
         Term result = replyTerm.dereference();
         return new PrologDocument(result, state, props);
      }
      System.out.println("Failed");
      return null;
   }

   public Term getInitialState(String component)
   {
      VariableTerm replyTerm = new VariableTerm("Result");
      Term goal = new CompoundTerm(AtomTerm.get("getInitialState_" + component), new Term[]{replyTerm});
      interpreter.undo(0);
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            // FIXME: Check that it is a list!
            return replyTerm.dereference();
         }
      }
      catch (PrologException notDefined)
      {
         notDefined.printStackTrace();
      }
      return TermConstants.emptyListAtom;
   }

   public void componentWillMount(String component)
   {
      Term goal = AtomTerm.get("componentWillMount_" + component);      
      interpreter.undo(0);
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
      }
      catch (PrologException notDefined)
      {
      }
   }

   public void componentWillUnmount(String component)
   {
      Term goal = AtomTerm.get("componentWillUnmount_" + component);      
      interpreter.undo(0);
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
      }
      catch (PrologException notDefined)
      {
      }
   }

   public Term instantiateProps(Map<String, Object> properties)
   {
      Term[] elements = new Term[properties.size()];
      int j = 0;
      for (Iterator<Map.Entry<String, Object>> i = properties.entrySet().iterator(); i.hasNext();)
      {
         Map.Entry<String, Object> entry = i.next();
         elements[j] = new CompoundTerm(CompoundTermTag.get(AtomTerm.get("="), 2),
                                        AtomTerm.get(entry.getKey()),
                                        (Term)entry.getValue());
         j++;
      }
      return CompoundTerm.getList(elements);
   }

   public static String asString(Object value)
   {      
      if (value instanceof AtomTerm)
         return ((AtomTerm)value).value;
      System.out.println("Invalid request for string version of " + value + "(" + value.getClass().getName() + ")");  
      return "";
   }
}
