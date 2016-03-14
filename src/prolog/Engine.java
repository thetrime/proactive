package org.proactive.prolog;

import org.proactive.ReactComponent;
import org.proactive.ReactWidget;

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
      env.installBuiltin("upcase_atom", 2);
      env.installBuiltin("format", 3);


      env.installBuiltin("on_server", 1);
      env.installBuiltin("raise_event", 2);
      env.installBuiltin("wait_for", 1);
      env.installBuiltin("get_this", 1);
      env.installBuiltin("react_handler", 3);

      env.installBuiltin("remove_child", 2);
      env.installBuiltin("append_child", 2);
      env.installBuiltin("insert_before", 3);
      env.installBuiltin("replace_child", 3);
      env.installBuiltin("child_nodes", 2);
      env.installBuiltin("create_element", 3);
      env.installBuiltin("create_text_node", 3);
      env.installBuiltin("parent_node", 2);
      env.installBuiltin("node_type", 2);
      env.installBuiltin("set_properties", 2);
      env.installBuiltin("replace_node_data", 2);
      env.installBuiltin("destroy_widget", 2);
      env.installBuiltin("init_widget", 3);
      env.installBuiltin("update_widget", 4);


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

   public Term getInitialState(String component, Term props)
   {
      VariableTerm replyTerm = new VariableTerm("Result");
      Term goal = ReactModule.crossModuleCall(component, new CompoundTerm(AtomTerm.get("getInitialState"), new Term[]{props, replyTerm}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            Term result = replyTerm.dereference().clone(new TermCloneContext());
            interpreter.undo(undoPosition);
            return result;
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

   public Term instantiateProps(Map<String, Term> properties)
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
      return CompoundTerm.getList(elements);
   }

   public Term fluxEvent(String componentName, Term key, Term value, Term state, Term props) throws Exception
   {
      VariableTerm newState = new VariableTerm("NewState");
      // We need to make sure that handlers dont (further) instantiate the key or value
      Term goal = ReactModule.crossModuleCall(componentName, new CompoundTerm(AtomTerm.get("handle_event"), new Term[]{key.clone(new TermCloneContext()),
                                                                                                                       value.clone(new TermCloneContext()),
                                                                                                                       state,
                                                                                                                       props,
                                                                                                                       newState}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            Term adjustedState = applyState(state, newState.dereference()).clone(new TermCloneContext());
            System.out.println("flux handler set state to: " + adjustedState);
            interpreter.undo(undoPosition);
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

   public boolean triggerEvent(Object handler, Term event, ReactWidget context) throws PrologException
   {
      Term state;
      Term props;
      if (handler instanceof CompoundTerm && ((CompoundTerm)handler).tag.functor.value.equals("react_handler"))
      {
         // Just call the handler directly and return
         Term goal;
         CompoundTerm c_handler = (CompoundTerm)handler;
         goal = ReactModule.crossModuleCall(context.getComponentName(), new CompoundTerm(c_handler.tag.functor, new Term[]{c_handler.args[0], c_handler.args[1], event}));
         //System.out.println("Executing " + goal);
         int undoPosition = interpreter.getUndoPosition();
         Interpreter.Goal g = interpreter.prepareGoal(goal);
         try
         {
            PrologCode.RC rc = interpreter.execute(g);
            if (rc == PrologCode.RC.SUCCESS)
               interpreter.stop(g);
            return (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST);
         }
         catch (PrologException notDefined)
         {
            notDefined.printStackTrace();
         }
         return false;
      }


      state = context.getState();
      props = context.getProps();

      Term goal;
      VariableTerm newState = new VariableTerm("NewState");
      if (handler instanceof AtomTerm)
         goal = ReactModule.crossModuleCall(context.getComponentName(), new CompoundTerm((AtomTerm)handler, new Term[]{event, state, props, newState}));
      else if (handler instanceof CompoundTerm)
      {
         CompoundTerm c_handler = (CompoundTerm)handler;
         Term[] args = new Term[c_handler.tag.arity + 4];
         for (int i = 0; i < c_handler.tag.arity; i++)
            args[i] = c_handler.args[i];
            //args[i] = unpack_recursive(c_handler.args[i]);
         args[c_handler.tag.arity+0] = event;
         args[c_handler.tag.arity+1] = state;
         args[c_handler.tag.arity+2] = props;
         args[c_handler.tag.arity+3] = newState;
         goal = ReactModule.crossModuleCall(context.getComponentName(), new CompoundTerm(c_handler.tag.functor, args));
      }
      else
      {
         System.out.println("Handler is not callable: " + handler);
         return false;
      }
      // This SHOULD be safe since we SHOULD always be at the top-level when doing this, and if not, we want to go there!
      //System.out.println("Executing " + goal);
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            //System.out.println("Goal " + goal + " has set the state to " + newState.dereference());
            context.setState(applyState(state, newState.dereference()));
            interpreter.undo(undoPosition);
            return (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST);
         }
      }
      catch (PrologException notDefined)
      {
         notDefined.printStackTrace();
      }
      return false;
      // State is not updated if we get to here
   }

   private Term applyState(Term oldState, Term newState) throws PrologException
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
   
   private void addProperties(Term state, Map<String, Term> props) throws PrologException
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
                  PrologException.typeError(AtomTerm.get("=/2"), attr);
               Term attrName = attr.args[0];
               Term attrValue = attr.args[1];
               if (!(attrName instanceof AtomTerm))
                  PrologException.typeError(AtomTerm.get("atom"), attrName);
               attrValue = attrValue.dereference();
               if (isNull(attrValue))
                  props.remove(((AtomTerm)attrName).value);
               else
                  props.put(((AtomTerm)attrName).value, attrValue.dereference());
            }
            else
               PrologException.typeError(AtomTerm.get("=/2"), c);
            if (c.args[1] instanceof CompoundTerm)
               c = (CompoundTerm)c.args[1];
            else if (TermConstants.emptyListAtom.equals(c.args[1]))
               break;
            else
               PrologException.typeError(AtomTerm.get("list"), c);
         }
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
               //System.out.println("Got: " + response);
            }
         }
         return state;
      }

      public void cut()
      {
         close();
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
      return new ExecutionState(goalURI, t, e);
   }


   public Term diff(Term a, Term b) throws PrologException
   {
      VariableTerm patchTerm = new VariableTerm("Patch");
      Term goal = ReactModule.crossModuleCall("diff", new CompoundTerm(AtomTerm.get("diff"), new Term[]{a, b, patchTerm}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      PrologCode.RC rc = interpreter.execute(g);
      if (rc == PrologCode.RC.SUCCESS)
         interpreter.stop(g);
      if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
      {
         Term result = patchTerm.dereference().clone(new TermCloneContext());
         interpreter.undo(undoPosition);
         return result;
      }
      return TermConstants.emptyListAtom;
   }

   public ReactComponent applyPatch(Term patch, ReactComponent root) throws PrologException
   {
      //System.out.println("Patching tree from " + root + " AWT: " + javax.swing.SwingUtilities.isEventDispatchThread());
      VariableTerm newRoot = new VariableTerm("NewRoot");
      Term renderOptions = CompoundTerm.getList(new Term[]{new CompoundTerm("document", new Term[]{new JavaObjectTerm(root.getOwnerDocument())})});
      Term goal = ReactModule.crossModuleCall("diff", new CompoundTerm(AtomTerm.get("patch"), new Term[]{new JavaObjectTerm(root),
                                                                                                         patch,
                                                                                                         renderOptions,
                                                                                                         newRoot}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      PrologCode.RC rc = interpreter.execute(g);
      if (rc == PrologCode.RC.SUCCESS)
         interpreter.stop(g);
      if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
      {
         JavaObjectTerm result = (JavaObjectTerm)(newRoot.dereference());
         ReactComponent returnValue = ((ReactComponent)result.value);
         interpreter.undo(undoPosition);
         return returnValue;
      }
      System.out.println(" *********************** patch/4 failed: " + patch);
      System.exit(-1);
      return null;
   }

   public Term render(ReactWidget widget, String component, Term state, Term props) throws PrologException
   {
      //System.out.println("Rendering " + component + " with props " + props + " and state " + state);
      VariableTerm vDom = new VariableTerm("VDom");
      Term goal = ReactModule.crossModuleCall(component, new CompoundTerm(AtomTerm.get("render"), new Term[]{state,
                                                                                                             props,
                                                                                                             vDom}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      env.pushContext(interpreter, widget);
      PrologCode.RC rc;
      try
      {
         rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            Term result = vDom.dereference();
            if (result instanceof CompoundTerm && ((CompoundTerm)result).tag.functor.value.equals("jsx") && ((CompoundTerm)result).tag.arity == 2)
            {
               CompoundTerm jsx = (CompoundTerm)result;
               rc = interpreter.runOnce(jsx.args[0]);
               if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
                  result = jsx.args[1].dereference();
               else
                  result = TermConstants.emptyListAtom;
            }
            result = result.clone(new TermCloneContext());
            interpreter.undo(undoPosition);
            return result;
         }
      }
      finally
      {
         env.popContext(interpreter);
      }
      return null;
   }

   public static HashMap<String, PrologObject> termToProperties(Term t) throws PrologException
   {
      HashMap<String, PrologObject> properties = new HashMap<String,PrologObject>();
      if (!TermConstants.emptyListAtom.equals(t))
      {
         if (t instanceof CompoundTerm)
         {
            CompoundTerm list = (CompoundTerm)t;
            while (list.tag == TermConstants.listTag && list.tag.arity == 2)
            {
               if (list.args[0] instanceof CompoundTerm)
               {
                  CompoundTerm attr = (CompoundTerm)list.args[0];
                  if (attr.tag.arity != 2 || !attr.tag.functor.value.equals("="))
                     PrologException.typeError(AtomTerm.get("attribute"), attr);
                  Term attrName = attr.args[0];
                  Term attrValue = attr.args[1];
                  if (attrName instanceof AtomTerm)
                     properties.put(((AtomTerm)attrName).value, new PrologObject(attrValue));
                  else
                     PrologException.typeError(AtomTerm.get("atom"), attrName);
                  if (TermConstants.emptyListAtom.equals(list.args[1]))
                     break;
                  else if (list.args[1] instanceof CompoundTerm)
                     list = (CompoundTerm)list.args[1];
                  else
                     PrologException.typeError(AtomTerm.get("list"), t);
               }
            }
         }
      }
      return properties;
   }

}
