import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;
import java.io.*;
import java.net.*;
import java.lang.reflect.*;
import java.util.concurrent.*;

// See https://github.com/TooTallNate/Java-WebSocket
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.drafts.Draft_17;
import org.java_websocket.handshake.ServerHandshake;

public class Engine
{
   private ReactEnvironment env;
   private Interpreter interpreter;
   private String componentURL;
   private String rootElementId;
   private URI goalURI;
   public Engine(String baseURL, String rootElementId) throws Exception
   {
      this.goalURI = new URI(baseURL + "/goal");
      this.componentURL = baseURL + "/component/";
      this.rootElementId = rootElementId;
      make();
   }   

   public void make() throws Exception
   {
      env = new ReactEnvironment(this);
      env.installBuiltin("java_println", 1);
      env.installBuiltin("on_server", 1);
      env.ensureLoaded(new CompoundTerm(CompoundTermTag.get("resource", 1), AtomTerm.get("/boilerplate.pl")));
      interpreter = env.createInterpreter();      
      env.ensureLoaded(componentURL, rootElementId);
      env.runInitialization(interpreter);
      env.linkModules();
      System.out.println("Checking for load errors...");
      List<PrologTextLoaderError> errors = env.getLoadingErrors();
      for (PrologTextLoaderError error : errors)
      {
         error.printStackTrace();
      }
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
      Term goal = ReactModule.crossModuleCall(component, new CompoundTerm(AtomTerm.get("render"), new Term[]{state, props, replyTerm}));
      System.out.println("Execute: " + goal);
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
      Term goal = ReactModule.crossModuleCall(component, new CompoundTerm(AtomTerm.get("getInitialState"), new Term[]{replyTerm}));
      interpreter.undo(0);
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            return new PrologState(replyTerm.dereference());
         }
      }
      catch (PrologException notDefined)         
      {
         notDefined.printStackTrace();
         System.exit(-1);
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

   public PrologState triggerEvent(String componentName, Object handler, PrologState stateWrapper, PrologState propsWrapper) throws Exception
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
         goal = ReactModule.crossModuleCall(componentName, new CompoundTerm((AtomTerm)handler, new Term[]{state, props, newState}));
      else if (handler instanceof CompoundTerm)
      {
         CompoundTerm c_handler = (CompoundTerm)handler;
         Term[] args = new Term[c_handler.tag.arity + 3];
         for (int i = 0; i < c_handler.tag.arity; i++)
            args[i] = unpack_recursive(c_handler.args[i]);
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

   // This adds quite a bit of overhead
   public static Term unpack_recursive(Term value)
   {
      if (value instanceof CompoundTerm)
      {
         CompoundTerm c = (CompoundTerm)value;
         if (c.tag.functor.value.equals("$state"))
            return unpack(c);
         
         Term[] args = new Term[c.args.length];
         for (int i = 0; i < c.args.length; i++)
            args[i] = unpack_recursive(c.args[i]);
         CompoundTerm replacement = new CompoundTerm(c.tag.functor.value, args);
         return replacement;
      }
      return value;
   }

   public static Term unpack(Term value)
   {
      if (value instanceof CompoundTerm)
      {
         if (((CompoundTerm)value).tag.functor.value.equals("$state"))
         {
            // Fake maps
            CompoundTerm c = (CompoundTerm)value;
            String key = ((AtomTerm)(c.args[0])).value;
            if (TermConstants.emptyListAtom.equals(c.args[1]))
               return value;
            else if (!(c.args[1] instanceof CompoundTerm))
               return value;                      
            CompoundTerm list = (CompoundTerm)c.args[1];
            while (list.tag.arity == 2 && list.tag.functor.value.equals("."))
            {
               Term head = list.args[0];
               if (head instanceof CompoundTerm && ((CompoundTerm)head).tag.functor.value.equals("=") && ((CompoundTerm)head).tag.arity == 2)
               {
                  CompoundTerm pair = (CompoundTerm)head;
                  if (pair.args[0] instanceof AtomTerm && ((AtomTerm)pair.args[0]).value.equals(key))
                     return pair.args[1];
               }
               if (list.args[1] instanceof CompoundTerm)
                  list = (CompoundTerm)list.args[1];
               else
                  return value;
            }
         }
      }
      return value;
   }
   
   public static String asString(Object value)
   {      
      if (value instanceof AtomTerm)
         return ((AtomTerm)value).value;
      else if (value instanceof CompoundTerm)
      {
         Term unpacked = unpack((CompoundTerm)value);
         if (unpacked instanceof AtomTerm)
            return ((AtomTerm)unpacked).value;
      }
      System.out.println("Invalid request for string version of " + value + "(" + value.getClass().getName() + ")");  
      return "";
   }

   public static class ExecutionState extends WebSocketClient
   {
      public enum RC
      {
         FAIL, EXCEPTION, SUCCESS_LAST, SUCCESS;
      }
      private RC state;
      private URLConnection connection;
      private Environment environment;
      //private TermParser input;
      
      ReadOptions options;
      private Term exception;
      private Term response;
      LinkedBlockingQueue<Term> replies = new LinkedBlockingQueue<Term>();
      public ExecutionState(URI uri, Term t, Environment e) throws IOException, InterruptedException
      {
         super(uri, new Draft_17());
         this.environment = e;
         options = new ReadOptions(e.getOperatorSet());
         connectBlocking();
         String goal = t.toString() + ".\n";
         send(goal);
      }
      public Term getException()
      {
         return exception;
      }
      public Term getResponse()
      {
         return response;
      }
      public RC nextSolution() throws InterruptedException
      {
         send(";");
         Term reply = replies.take();
         if (reply instanceof AtomTerm && ((AtomTerm)reply).value.equals("fail"))
         {
            close();
            state = RC.FAIL;
         }
         else if (reply instanceof CompoundTerm)
         {
            CompoundTerm c = (CompoundTerm)reply;
            if (c.tag.functor.value.equals("exception"))
            {
               close();
               state = RC.EXCEPTION;
               exception = c.args[0];
            }
            else
            {
               if (c.tag.functor.value.equals("cut"))
               {
                  close();
                  state = RC.SUCCESS_LAST;
               }
               else
                  state = RC.SUCCESS;
               response = c.args[0];
               System.out.println("Got: " + response);
            }
         }
         return state;
      }
   
      @Override
      public void onMessage(String message)
      {
         try
         {
            replies.put(TermReader.stringToTerm(options, message, environment));
         }
         catch(InterruptedException | ParseException e)
         {
            // FIXME: Should actually poke an exception term onto the queue, but... that could just raise another exception. hmm.
            e.printStackTrace();
         }
      }
      
      @Override
      public void onOpen(ServerHandshake handshake)
      {
         System.out.println("opened connection");
      }
      
      @Override
      public void onClose(int code, String reason, boolean remote)
      {
         System.out.println("closed connection");
      }
      
      @Override
      public void onError(Exception ex)
      {
         ex.printStackTrace();
      }      
   }
   
   public ExecutionState prepareGoal(Term t, Environment e) throws IOException, InterruptedException
   {
      System.out.println(goalURI);
      return new ExecutionState(goalURI, t, e);
   }
    

}
