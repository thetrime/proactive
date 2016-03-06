package org.proactive.prolog;

import org.proactive.vdom.PrologNode;
import org.proactive.vdom.PrologDocument;
import org.proactive.ReactComponent;

import gnu.prolog.database.PrologTextLoaderError;
import gnu.prolog.io.ReadOptions;
import gnu.prolog.io.ParseException;
import gnu.prolog.io.TermReader;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.JavaObjectTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.TermCloneContext;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.term.CompoundTermTag;
import gnu.prolog.term.Term;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.vm.PrologCode;
import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.Environment;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.io.IOException;
import java.net.URI;
import java.util.concurrent.LinkedBlockingQueue;

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
      long t1 = System.currentTimeMillis();
      env = new ReactEnvironment(this);
      env.installBuiltin("java_println", 1);
      env.installBuiltin("on_server", 1);
      env.installBuiltin("raise_event", 2);
      env.installBuiltin("wait_for", 1);

      env.installBuiltin("remove_child", 2);
      env.installBuiltin("append_child", 2);
      env.installBuiltin("insert_before", 3);
      env.installBuiltin("replace_child", 3);
      env.installBuiltin("child_nodes", 2);
      env.installBuiltin("create_element", 3);
      env.installBuiltin("create_text_node", 3);
      env.installBuiltin("parent_node", 2);
      env.installBuiltin("node_type", 2);
      env.installBuiltin("set_property", 3);
      env.installBuiltin("replace_node_data", 2);
      env.installBuiltin("destroy_widget", 2);


      env.ensureLoaded(new CompoundTerm(CompoundTermTag.get("resource", 1), AtomTerm.get("/boilerplate.pl")));
      env.ensureLoaded(new CompoundTerm(CompoundTermTag.get("resource", 1), AtomTerm.get("/diff.pl")));
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
      System.out.println("Compile time: " + (System.currentTimeMillis() - t1) + "ms");
   }   
   
   public PrologDocument render(String component, PrologState stateWrapper, PrologState propsWrapper, PrologContext parentContext) throws Exception
   {
      //System.out.println("Rendering " + component);
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
      //System.out.println("Execute: " + goal);
      // We cannot undo(0) here because we might be in the middle of processing a flux event, and that would muck up the stack
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      PrologCode.RC rc = interpreter.execute(g);
      if (rc == PrologCode.RC.SUCCESS)
         interpreter.stop(g);
      if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
      {
         Term result = replyTerm.dereference();
         PrologDocument doc = new PrologDocument(result, state, props, component, this, parentContext);
         return doc;
      }
      System.out.println("Failed to render");
      return null;
   }

   public Term getInitialState(String component, Term props)
   {
      VariableTerm replyTerm = new VariableTerm("Result");
      Term goal = ReactModule.crossModuleCall(component, new CompoundTerm(AtomTerm.get("getInitialState"), new Term[]{props, replyTerm}));
      // We cannot undo(0) here because we might be in the middle of processing a flux event, and that would muck up the stack
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            return replyTerm.dereference();
         }
      }
      catch (PrologException notDefined)         
      {
          // FIXME: There are two possible exceptions here. One is that there is no such predicate - that is OK. The other is there is no such module - that is bad.
          // Really we should be checking for the predicates existence first, somehow...
         //notDefined.printStackTrace();
         //System.exit(-1);
      }      
      return TermConstants.emptyListAtom;
   }

   public PrologState instantiateProps(Map<String, Term> properties)
   {
      Term[] elements = new Term[properties.size()];
      int j = 0;
      for (Iterator<Map.Entry<String, Term>> i = properties.entrySet().iterator(); i.hasNext();)
      {
         Map.Entry<String, Term> entry = i.next();
         elements[j] = new CompoundTerm(CompoundTermTag.get(AtomTerm.get("="), 2),
                                        AtomTerm.get(entry.getKey()),
                                        (Term)entry.getValue());
         j++;
      }
      return new PrologState(CompoundTerm.getList(elements));
   }

   public PrologState fluxEvent(String componentName, Term key, Term value, PrologState stateWrapper, PrologState propsWrapper) throws Exception
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
      // We need to make sure that handlers dont (further) instantiate the key or value
      Term goal = ReactModule.crossModuleCall(componentName, new CompoundTerm(AtomTerm.get("handle_event"), new Term[]{key.clone(new TermCloneContext()),
                                                                                                                       value.clone(new TermCloneContext()),
                                                                                                                       state,
                                                                                                                       props,
                                                                                                                       newState}));
      // We cannot call undo(0) here because we might be processing recursive events
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            PrologState adjustedState = applyState(state, newState.dereference());
            System.out.println("flux handler set state to: " + adjustedState);
            return adjustedState;
         }
      }
      catch (PrologException notDefined)         
      {
          // FIXME: There are two possible exceptions here. One is that there is no such predicate - that is OK. The other is there is no such module - that is bad.
          // Really we should be checking for the predicates existence first, somehow...
         notDefined.printStackTrace();
         System.exit(-1);
      }
      return null;
   }
   
   public void triggerEvent(Object handler, PrologObject event, PrologContext context) throws Exception
   {
      Term state;
      Term props;

      System.out.println("Handler: " + handler);
      if (handler instanceof JavaObjectTerm && (((JavaObjectTerm)handler).value instanceof BoundHandler))
      {
         BoundHandler boundHandler = (BoundHandler)((JavaObjectTerm)handler).value;
         // Context switch!
         context = boundHandler.context;
         handler = boundHandler.handler;
      }

      PrologState stateWrapper = context.state;
      PrologState propsWrapper = context.props;


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
      if (handler instanceof AtomTerm)
         goal = ReactModule.crossModuleCall(context.componentName, new CompoundTerm((AtomTerm)handler, new Term[]{event.asTerm(), state, props, newState}));
      else if (handler instanceof CompoundTerm)
      {
         CompoundTerm c_handler = (CompoundTerm)handler;
         Term[] args = new Term[c_handler.tag.arity + 4];
         for (int i = 0; i < c_handler.tag.arity; i++)
            args[i] = c_handler.args[i];
            //args[i] = unpack_recursive(c_handler.args[i]);
         args[c_handler.tag.arity+0] = event.asTerm();
         args[c_handler.tag.arity+1] = state;
         args[c_handler.tag.arity+2] = props;
         args[c_handler.tag.arity+3] = newState;
         goal = ReactModule.crossModuleCall(context.componentName, new CompoundTerm(c_handler.tag.functor, args));
      }
      else
      {
         System.out.println("Handler is not callable: " + handler);
         return;
      }
      // This SHOULD be safe since we SHOULD always be at the top-level when doing this, and if not, we want to go there!
      interpreter.undo(0);
      System.out.println("Executing " + goal);
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            context.setState(applyState(state, newState.dereference()));
            return;
         }
      }
      catch (PrologException notDefined)
      {
         notDefined.printStackTrace();
      }
      // State is not updated if we get to here
   }

   private PrologState applyState(Term oldState, Term newState) throws Exception
   {
      Map<String, Term> properties = new HashMap<String, Term>();
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
   
   private void addProperties(Term state, Map<String, Term> props) throws Exception
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
   public static Term unpack_recursive(Term value, PrologContext context)
   {
      if (value instanceof CompoundTerm)
      {
         CompoundTerm c = (CompoundTerm)value;
         if (c.tag.functor.value.equals("$state"))
            return unpack(c, context);
         
         Term[] args = new Term[c.args.length];
         for (int i = 0; i < c.args.length; i++)
            args[i] = unpack_recursive(c.args[i], context);
         CompoundTerm replacement = new CompoundTerm(c.tag.functor.value, args);
         return replacement;
      }
      return value;
   }

   public static Term unpack(Term value, PrologContext context)
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
                     return unpack(pair.args[1].dereference(), context);
               }
               if (list.args[1] instanceof CompoundTerm)
                  list = (CompoundTerm)list.args[1];
               else
               {
                  // Entry is not found.
                  return null;
               }
            }
         }
         else if (((CompoundTerm)value).tag.functor.value.equals("$this"))
         {
            System.out.println("Unpacking a this pointer in " + context.componentName);
            System.out.println("Handler is " +unpack(((CompoundTerm)value).args[0], context));
            return new JavaObjectTerm(new BoundHandler(unpack(((CompoundTerm)value).args[0], context), context));
         }
      }
      return value;
   }

   public static class BoundHandler
   {
      public Term handler;
      public PrologContext context;
      public BoundHandler(Term handler, PrologContext context)
      {
         this.handler = handler;
         this.context = context;
      }
   }

   public static class ExecutionState extends WebSocketClient
   {
      public enum RC
      {
         FAIL, EXCEPTION, SUCCESS_LAST, SUCCESS;
      }
      private RC state;
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
         String goal = gnu.prolog.io.TermWriter.toString(t) + ".\n";
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


   public Term diff(Term a, Term b) throws PrologException
   {
      VariableTerm patchTerm = new VariableTerm("Patch");
      Term goal = ReactModule.crossModuleCall("diff", new CompoundTerm(AtomTerm.get("diff"), new Term[]{a, b, patchTerm}));
      // FIXME: Maybe...We cannot undo(0) here because we might be in the middle of processing a flux event, and that would muck up the stack
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      PrologCode.RC rc = interpreter.execute(g);
      if (rc == PrologCode.RC.SUCCESS)
         interpreter.stop(g);
      if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
      {
         return patchTerm.dereference();
      }
      return TermConstants.emptyListAtom;
   }

   public ReactComponent applyPatch(Term patch, ReactComponent root) throws PrologException
   {
      VariableTerm newRoot = new VariableTerm("NewRoot");
      Term goal = ReactModule.crossModuleCall("diff", new CompoundTerm(AtomTerm.get("patch"), new Term[]{new JavaObjectTerm(root),
                                                                                                         patch,
                                                                                                         TermConstants.emptyListAtom,
                                                                                                         newRoot}));
      // FIXME: Maybe...We cannot undo(0) here because we might be in the middle of processing a flux event, and that would muck up the stack
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      PrologCode.RC rc = interpreter.execute(g);
      if (rc == PrologCode.RC.SUCCESS)
         interpreter.stop(g);
      if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
      {
         JavaObjectTerm result = (JavaObjectTerm)(newRoot.dereference());
         return ((ReactComponent)result.value);
      }
      return null;
   }

   public Term render(String component, Term state, Term props) throws Exception
   {
      VariableTerm vDom = new VariableTerm("VDom");
      Term goal = ReactModule.crossModuleCall(component, new CompoundTerm(AtomTerm.get("render"), new Term[]{state,
                                                                                                             props,
                                                                                                             vDom}));
      // FIXME: Maybe...We cannot undo(0) here because we might be in the middle of processing a flux event, and that would muck up the stack
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      PrologCode.RC rc = interpreter.execute(g);
      if (rc == PrologCode.RC.SUCCESS)
         interpreter.stop(g);
      if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
      {
         return vDom.dereference();
      }
      return null;
   }

}
