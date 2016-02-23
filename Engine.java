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
      env.ensureLoaded(AtomTerm.get("boilerplate.pl"));
      env.ensureLoaded(AtomTerm.get("sample.pl"));
      interpreter = env.createInterpreter();
      installBuiltin("java_println", 1);
      System.out.println("Checking for load errors...");
      List<PrologTextLoaderError> errors = env.getLoadingErrors();
      for (PrologTextLoaderError error : errors)
      {
         error.printStackTrace();
      }
   }

   public void installBuiltin(String functor, int arity) throws PrologException
   {
      Module module = env.getModule();
      CompoundTermTag head = CompoundTermTag.get(AtomTerm.get(functor), arity);
      Predicate p = module.createDefinedPredicate(head);
      p.setType(Predicate.TYPE.BUILD_IN);
      p.setJavaClassName("Predicate_" + functor);
      PrologCode q = env.loadPrologCode(head);
   }
   
   public PrologDocument render(String component, PrologState stateWrapper, PrologState propsWrapper) throws Exception
   {
      System.out.println("Rendering " + component);
      Term state;
      Term props;
      if (stateWrapper == null)
         state = TermConstants.emptyListAtom;
      else
         state = stateWrapper.getValue();
      if (propsWrapper == null)
         props = TermConstants.emptyListAtom;
      else
         props = propsWrapper.getValue();
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
         return new PrologDocument(result, state, props, component);
      }
      System.out.println("Failed");
      return null;
   }

   public PrologState getInitialState(String component)
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
            return new PrologState(replyTerm.dereference());
         }
      }
      catch (PrologException notDefined)
      {
      }
      return PrologState.emptyState();
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

   public PrologState instantiateProps(Map<String, Object> properties)
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
      return new PrologState(CompoundTerm.getList(elements));
   }

   public PrologState triggerEvent(Object handler, PrologState stateWrapper, PrologState propsWrapper)
   {
      Term state;
      Term props;
      if (stateWrapper == null)
         state = TermConstants.emptyListAtom;
      else
         state = stateWrapper.getValue();
      if (propsWrapper == null)
         props = TermConstants.emptyListAtom;
      else
         props = propsWrapper.getValue();
      // FIXME: handler may not be an atom
      VariableTerm newState = new VariableTerm("NewState");
      Term goal = new CompoundTerm((AtomTerm)handler, new Term[]{state, props, newState});
      interpreter.undo(0);
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {            
            return applyState(state, newState.dereference());
         }
      }
      catch (PrologException notDefined)
      {
         notDefined.printStackTrace();
      }
      return null;
   }

   private PrologState applyState(Term oldState, Term newState)
   {
      // FIXME: Need to actually apply changes here
      return new PrologState(newState);
   }


   
   public static String asString(Object value)
   {      
      if (value instanceof AtomTerm)
         return ((AtomTerm)value).value;
      System.out.println("Invalid request for string version of " + value + "(" + value.getClass().getName() + ")");  
      return "";
   }
}
