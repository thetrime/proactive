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
   private String URL;
   public Engine(String URL) throws Exception
   {
      this.URL = URL;
      make();
   }

   public void make() throws Exception
   {
      env = new Environment();
      env.ensureLoaded(AtomTerm.get("boilerplate.pl"));
      env.ensureLoaded(new CompoundTerm(CompoundTermTag.get("url", 1),
                                        AtomTerm.get(URL)));
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
         return new PrologDocument(result, state, props, component, this);
      }
      System.out.println("Failed to render");
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
            return new PrologState(replyTerm.dereference());
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

   public PrologState triggerEvent(Object handler, PrologState stateWrapper, PrologState propsWrapper) throws Exception
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
      VariableTerm newState = new VariableTerm("NewState");
      Term goal;
      System.out.println("Handler: " + handler);
      if (handler instanceof AtomTerm)
         goal = new CompoundTerm((AtomTerm)handler, new Term[]{state, props, newState});
      else if (handler instanceof CompoundTerm)
      {
         CompoundTerm c_handler = (CompoundTerm)handler;
         Term[] args = new Term[c_handler.tag.arity + 3];
         for (int i = 0; i < c_handler.tag.arity; i++)
            args[i] = c_handler.args[i];
         args[c_handler.tag.arity+0] = state;
         args[c_handler.tag.arity+1] = props;
         args[c_handler.tag.arity+2] = newState;         
         goal = new CompoundTerm(c_handler.tag.functor, args);
      }
      else
      {
         System.out.println("Handler is not callable: " + handler);
         return null;
      }      
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

   private PrologState applyState(Term oldState, Term newState) throws Exception
   {
      Map<String, Object> properties = new HashMap<String, Object>();
      addProperties(oldState, properties);
      addProperties(newState, properties);
      return instantiateProps(properties);
   }

   private static boolean isNull(Term t)
   {
      if (t instanceof CompoundTerm)
      {
         CompoundTerm c = (CompoundTerm)t;
         if (c.tag == CompoundTermTag.curly1 && c.args[0] instanceof AtomTerm && "null".equals(((AtomTerm)c.args[0]).value))
            return true;
      }
      return false;
   }
   
   private void addProperties(Term state, Map<String, Object> props) throws Exception
   {
      if (TermConstants.emptyListAtom.equals(state))
         return;
      else if (state instanceof CompoundTerm)
      {
         CompoundTerm c = (CompoundTerm)state;
         while(c.tag.arity == 2)
         {
            if (c.args[0] instanceof CompoundTerm)
            {
               CompoundTerm attr = (CompoundTerm)c.args[0];
               if (attr.tag.arity != 2 || !attr.tag.functor.value.equals("="))                     
                  throw new RuntimeException("Invalid state: element is not =/2: " + attr);
               Term attrName = attr.args[0];
               Term attrValue = attr.args[1];
               if (!(attrName instanceof AtomTerm))
                  throw new RuntimeException("Invalid state: element name is not an atom: " + attrName);
               attrValue = attrValue.dereference();
               if (isNull(attrValue))
                  props.remove(((AtomTerm)attrName).value);
               else
                  props.put(((AtomTerm)attrName).value, attrValue.dereference());
            }
            else
               throw new RuntimeException("Invalid state element: " + c);
            if (c.args[1] instanceof CompoundTerm)
               c = (CompoundTerm)c.args[1];
            else if (TermConstants.emptyListAtom.equals(c.args[1]))
               break;
            else
               throw new RuntimeException("Invalid state. Not a list: " + c);
         }
      }
   }


   
   public static String asString(Object value)
   {      
      if (value instanceof AtomTerm)
         return ((AtomTerm)value).value;
      if (value instanceof CompoundTerm)
      {
         if (((CompoundTerm)value).tag.functor.value.equals("$state"))
         {
            // Fake maps
            CompoundTerm c = (CompoundTerm)value;
            String key = ((AtomTerm)(c.args[0])).value;
            if (TermConstants.emptyListAtom.equals(c.args[1]))
               return "";
            else if (!(c.args[1] instanceof CompoundTerm))
               return "";                        
            CompoundTerm list = (CompoundTerm)c.args[1];
            while (list.tag.arity == 2 && list.tag.functor.value.equals("."))
            {
               Term head = list.args[0];
               if (head instanceof CompoundTerm && ((CompoundTerm)head).tag.functor.value.equals("=") && ((CompoundTerm)head).tag.arity == 2)
               {
                  CompoundTerm pair = (CompoundTerm)head;
                  if (pair.args[0] instanceof AtomTerm && ((AtomTerm)pair.args[0]).value.equals(key))
                     return asString(pair.args[1]);
               }
               if (list.args[1] instanceof CompoundTerm)
                  list = (CompoundTerm)list.args[1];
               else
                  return "";
            }
         }
      }
      System.out.println("Invalid request for string version of " + value + "(" + value.getClass().getName() + ")");  
      return "";
   }
}
