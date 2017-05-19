package org.proactive.prolog;

import org.proactive.ReactComponent;
import org.proactive.ReactWidget;
import org.proactive.React;
import org.proactive.HTTPContext;

import gnu.prolog.database.PrologTextLoaderError;
import gnu.prolog.database.Module;
import gnu.prolog.database.MetaPredicateInfo;
import gnu.prolog.database.Predicate;
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
import java.util.Vector;
import java.util.List;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.net.MalformedURLException;
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
   private URI listenURI;
   private URI goalURI;
   private FluxDispatcher fluxDispatcher = new FluxDispatcher();
   private HTTPContext httpContext;
   private Map<String, String> emptyHTTPHeaders = new HashMap<String, String>();
   private ReactWidget rootWidget = null;
   private WebSocketClient serverConnection = null;
   private Map<String, List<ReactWidget>> knownWidgets = new HashMap<String, List<ReactWidget>>();
   private static final CompoundTermTag consultedFunctor = CompoundTermTag.get("consulted", 1);
   private static final CompoundTermTag systemFunctor = CompoundTermTag.get("system", 1);
   private static final CompoundTermTag messageFunctor = CompoundTermTag.get("message", 3);

   public Engine(String baseURL, String rootElementId, HTTPContext httpContext) throws Exception
   {
      goalURI = new URI(baseURL + "/goal");
      this.httpContext = httpContext;
      String scheme = "ws";
      if (goalURI.getScheme().toLowerCase().equals("https"))
         scheme = "wss";

      goalURI = new URI(scheme, goalURI.getUserInfo(), goalURI.getHost(), goalURI.getPort(), goalURI.getPath(), goalURI.getQuery(), goalURI.getFragment());
      this.componentURL = baseURL + "/component/";
      this.listenURI = new URI(baseURL + "/listen");
      listenURI = new URI(scheme, listenURI.getUserInfo(), listenURI.getHost(), listenURI.getPort(), listenURI.getPath(), listenURI.getQuery(), listenURI.getFragment());
      this.rootElementId = rootElementId;
      serverConnection = new ServerConnection(listenURI);
      make();
   }

   public URL relativeURL(String rel) throws MalformedURLException
   {
      return new URL(new URL(componentURL), rel);
   }

   // This creates an engine for testing
   static public void runTests() throws Exception
   {
      Environment e = boil(null, null);
      e.ensureLoaded(AtomTerm.get("test.pl"));
      Term goal = AtomTerm.get("run_all_tests");
      Interpreter i = e.createInterpreter();
      Interpreter.Goal g = i.prepareGoal(goal);
      PrologCode.RC rc = i.execute(g);
      if (rc == PrologCode.RC.SUCCESS)
         i.stop(g);
      if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         System.out.println("Success");
      else
         System.out.println("Failed");
   }


   public URI getListenURI()
   {
      return listenURI;
   }

   static ReactEnvironment boil(Engine e, HTTPContext c) throws Exception
   {
      ReactEnvironment newEnv = new ReactEnvironment(e, c);
      newEnv.installBuiltin("upcase_atom", 2);
      newEnv.installBuiltin("format", 3);
      Predicate p = newEnv.installBuiltin("findall", 4);
      p.setMeta(new MetaPredicateInfo(new MetaPredicateInfo.MetaType[]{MetaPredicateInfo.MetaType.NORMAL,
								       MetaPredicateInfo.MetaType.META,
								       MetaPredicateInfo.MetaType.NORMAL,
								       MetaPredicateInfo.MetaType.NORMAL}));


      newEnv.installBuiltin(".", 3);
      newEnv.installBuiltin("state_to_term", 2);
      newEnv.installBuiltin("_on_server", 1);
      newEnv.installBuiltin("raise_event", 2);
      newEnv.installBuiltin("wait_for", 1);
      newEnv.installBuiltin("get_this", 1);
      newEnv.installBuiltin("get_store_state", 2);
      newEnv.installBuiltin("bubble_event", 2);

      newEnv.installBuiltin("remove_child", 2);
      newEnv.installBuiltin("append_child", 2);
      newEnv.installBuiltin("insert_before", 3);
      newEnv.installBuiltin("replace_child", 3);
      newEnv.installBuiltin("child_nodes", 2);
      newEnv.installBuiltin("create_element", 3);
      newEnv.installBuiltin("create_text_node", 3);
      newEnv.installBuiltin("parent_node", 2);
      newEnv.installBuiltin("node_type", 2);
      newEnv.installBuiltin("set_vdom_properties", 2);
      newEnv.installBuiltin("replace_node_data", 2);
      newEnv.installBuiltin("destroy_widget", 2);
      newEnv.installBuiltin("destroy_component", 2);
      newEnv.installBuiltin("init_widget", 3);
      newEnv.installBuiltin("update_widget", 4);
      newEnv.installBuiltin("widget_id", 1);
      newEnv.installBuiltin("nth0", 3);
      newEnv.installBuiltin("memberchk", 2);
      newEnv.installBuiltin("writeln", 1);
      newEnv.installBuiltin("nb_setarg", 3);
      newEnv.installBuiltin("succ", 2);
      newEnv.installBuiltin("callable", 1);
      newEnv.installBuiltin("atomic_list_concat", 3);
      newEnv.installBuiltin("code_type", 2);
      newEnv.installBuiltin("ground", 1);
      newEnv.installBuiltin("broadcast_proactive_message", 1);
      newEnv.installBuiltin("media_size", 2);


      newEnv.ensureLoaded(new CompoundTerm(CompoundTermTag.get("resource", 1), AtomTerm.get("/boilerplate.pl")));
      newEnv.ensureLoaded(new CompoundTerm(CompoundTermTag.get("resource", 1), AtomTerm.get("/boilerplate_gpj.pl")));
      newEnv.ensureLoaded(new CompoundTerm(CompoundTermTag.get("resource", 1), AtomTerm.get("/vdiff.pl")));
      return newEnv;
   }

   public void make() throws Exception
   {
      long t1 = System.currentTimeMillis();
      env = boil(this, httpContext);
      interpreter = env.createInterpreter();
      env.ensureLoaded(componentURL, rootElementId);
      env.runInitialization(interpreter);
      fluxDispatcher.initializeFlux(this);
      System.out.println("Checking for load errors...");
      List<PrologTextLoaderError> errors = env.getLoadingErrors();
      for (PrologTextLoaderError error : errors)
      {
         error.printStackTrace();
      }
      System.out.println("Compile time: " + (System.currentTimeMillis() - t1) + "ms");
   }

   public PrologState getInitialState(String component, PrologState props)
   {
      if (!env.predicateExists(component, CompoundTermTag.get("getInitialState", 2)))
      {
	 return PrologState.emptyState;
      }
      VariableTerm replyTerm = new VariableTerm("Result");
      Term goal = Module.crossModuleCall(component, new CompoundTerm(AtomTerm.get("getInitialState"), new Term[]{props, replyTerm}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
	    PrologState result = new PrologState(replyTerm.dereference());
            interpreter.undo(undoPosition);
            return result;
         }
      }
      catch (PrologException notDefined)         
      {
	 notDefined.printStackTrace();
	 //System.exit(-1);
      }      
      return PrologState.emptyState;
   }

   public boolean componentWillReceiveProps(String component, ReactWidget context) throws PrologException
   {
      if (!env.predicateExists(component, CompoundTermTag.get("componentWillReceiveProps", 3)))
         return false;

      PrologState state = context.getState();
      PrologState props = context.getProps();
      VariableTerm newState = new VariableTerm("NewState");
      Term goal = Module.crossModuleCall(component, new CompoundTerm(AtomTerm.get("componentWillReceiveProps"), new Term[]{state, props, newState}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            // We must call getState() here since the state might have changed since we started! A series of events might have fired (specifically, Flux)
            // which might have mutated the state from its value when we started this event
            // FIXME: Is this really true for componentWillReceiveProps?
            PrologState finalState = applyState(context.getState(), newState.dereference());
            context.setState(finalState);
            return (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST);
	 }
      }
      finally
      {
         interpreter.undo(undoPosition);
      }
      return false;
   }

   public PrologState getInitialStoreState(String component)
   {
      VariableTerm replyTerm = new VariableTerm("Result");
      Term goal = Module.crossModuleCall(component, new CompoundTerm(AtomTerm.get("getInitialStoreState"), new Term[]{replyTerm}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
	    PrologState result = new PrologState(replyTerm.dereference().clone(new TermCloneContext()));
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
      return PrologState.emptyState;
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

   public Term checkForMessageHandlers(ReactWidget context, Term existingListeners)
   {
      if (!env.predicateExists(context.getComponentName(), CompoundTermTag.get("onMessage", 5)))
         return TermConstants.emptyListAtom;
      VariableTerm replyTerm = new VariableTerm("Result");
      VariableTerm deltaTerm = new VariableTerm("Delta");
      Term goal = new CompoundTerm(AtomTerm.get("updateMessageHandlers"), new Term[]{AtomTerm.get(context.getComponentName()),
                                                                                     existingListeners,
                                                                                     context.getState(),
                                                                                     context.getProps(),
                                                                                     deltaTerm,
                                                                                     replyTerm});
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            registerForMessages(deltaTerm.dereference());
            Term newListeners = replyTerm.dereference().clone(new TermCloneContext());
            interpreter.undo(undoPosition);
            return newListeners;
         }
         else
         {
            // No changes
            return existingListeners;
         }
      }
      catch (PrologException error)
      {
         error.printStackTrace();
      }
      return existingListeners;
   }

   public boolean checkForFluxListeners(ReactWidget context)
   {
      // FIXME: Not quite. listen_for needs to have a goal as the second argument, and we need to take note of that!
      VariableTerm storeName = new VariableTerm("Module");
      VariableTerm goalName = new VariableTerm("Goal");
      Term goal = Module.crossModuleCall(context.getComponentName(), new CompoundTerm(AtomTerm.get("listen_for"), new Term[]{storeName, goalName}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      boolean hasListener = false;
      try
      {
         while (true)
         {
            PrologCode.RC rc = interpreter.execute(g);
	    if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
	    {
	       fluxDispatcher.registerFluxListener(((AtomTerm)(storeName.dereference())).value, goalName.dereference(), context);
	       hasListener = true;
	    }
            if (rc == PrologCode.RC.FAIL || rc == PrologCode.RC.SUCCESS_LAST)
               break;
         }
      }
      catch (PrologException notDefined)
      {
         // Thats ok
      }
      return hasListener;
   }

   public PrologState getStoreState(String storeName)
   {
      return fluxDispatcher.getStoreState(storeName);
   }


   public boolean fluxEvent(Term handler, String storeName, PrologState storeState, ReactWidget context) throws Exception
   {
      PrologState state = context.getState();
      PrologState props = context.getProps();
      Term goal;
      VariableTerm newState = new VariableTerm("NewState");
      if (handler instanceof AtomTerm)
	 goal = Module.crossModuleCall(context.getComponentName(), new CompoundTerm((AtomTerm)handler, new Term[]{storeState,
														       state,
														       props,
														       newState}));
      else if (handler instanceof CompoundTerm)
      {
         CompoundTerm c_handler = (CompoundTerm)handler;
         Term[] args = new Term[c_handler.tag.arity + 5];
         for (int i = 0; i < c_handler.tag.arity; i++)
            args[i] = c_handler.args[i];
         args[c_handler.tag.arity+0] = AtomTerm.get(storeName);
	 args[c_handler.tag.arity+1] = storeState;
	 args[c_handler.tag.arity+2] = state;
	 args[c_handler.tag.arity+3] = props;
	 args[c_handler.tag.arity+4] = newState;
         goal = Module.crossModuleCall(context.getComponentName(), new CompoundTerm(c_handler.tag.functor, args));
      }
      else
      {
         System.out.println("Handler is not callable: " + handler);
         return false;
      }
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
            //System.out.println("Goal " + goal + " has set the state of " + context + " to " + newState.dereference());
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

   }

   public static final CompoundTermTag tagThis = CompoundTermTag.get("$this", 2);

   public Environment getEnvironment()
   {
      return env;
   }

   public Term renderContextualElement(Term handler, Term event, ReactWidget context) throws PrologException
   {
      PrologState state;
      PrologState props;
      if (handler instanceof CompoundTerm && ((CompoundTerm)handler).tag == tagThis)
      {
         CompoundTerm handlerTerm = (CompoundTerm)handler;
         ReactWidget target = (ReactWidget)((JavaObjectTerm)(handlerTerm.args[0])).value;
         Term newHandler = handlerTerm.args[1];
         return renderContextualElement(newHandler, event, target);
      }
      state = context.getState();
      props = context.getProps();

      Term goal;
      VariableTerm result = new VariableTerm("Result");
      if (handler instanceof AtomTerm)
	 goal = Module.crossModuleCall(context.getComponentName(), new CompoundTerm((AtomTerm)handler, new Term[]{event, state, props, result}));
      else if (handler instanceof CompoundTerm)
      {
         CompoundTerm c_handler = (CompoundTerm)handler;
         Term[] args = new Term[c_handler.tag.arity + 4];
         for (int i = 0; i < c_handler.tag.arity; i++)
            args[i] = c_handler.args[i];
         args[c_handler.tag.arity+0] = event;
	 args[c_handler.tag.arity+1] = state;
	 args[c_handler.tag.arity+2] = props;
         args[c_handler.tag.arity+3] = result;
         goal = Module.crossModuleCall(context.getComponentName(), new CompoundTerm(c_handler.tag.functor, args));
      }
      else
      {
         PrologException.typeError(AtomTerm.get("callable"), handler);
         return null; // Not really reachable
      }
      //System.out.println("Executing " + goal);
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      env.pushContext(interpreter, context);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            //System.out.println("Goal " + goal + " has set the state to " + newState.dereference());
            Term copy = (Term)result.dereference().clone();
            interpreter.undo(undoPosition);
            //System.out.println("Executed " + goal);
            return copy;
         }
      }
      catch (PrologException notDefined)
      {
         notDefined.printStackTrace();
      }
      finally
      {
         env.popContext(interpreter);
      }
      return TermConstants.emptyListAtom;
   }

   public ReactComponent createElementFromVDom(Term vDOM, ReactWidget context) throws PrologException
   {
      VariableTerm resultValue = new VariableTerm("Result");
      Term renderOptions = CompoundTerm.getList(new Term[]{new CompoundTerm("document", new Term[]{new JavaObjectTerm(context)})});
      Term goal = Module.crossModuleCall("vdiff", new CompoundTerm(AtomTerm.get("create_element_from_vdom"), new Term[]{renderOptions,
                                                                                                                            vDOM,
                                                                                                                            resultValue}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      PrologCode.RC rc = interpreter.execute(g);
      if (rc == PrologCode.RC.SUCCESS)
         interpreter.stop(g);
      if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
      {
         JavaObjectTerm result = (JavaObjectTerm)(resultValue.dereference());
         ReactComponent returnValue = ((ReactComponent)result.value);
         interpreter.undo(undoPosition);
         return returnValue;
      }
      System.out.println(" *********************** create_element_from_vdom/3 failed: " + vDOM);
      //System.exit(-1);
      return null;
   }

   public boolean triggerEvent(Term handler, Term event, ReactWidget context) throws PrologException
   {
      PrologState state;
      PrologState props;
      if (handler instanceof CompoundTerm && ((CompoundTerm)handler).tag == tagThis)
      {
         CompoundTerm handlerTerm = (CompoundTerm)handler;
         ReactWidget target = (ReactWidget)((JavaObjectTerm)(handlerTerm.args[0])).value;
         Term newHandler = handlerTerm.args[1];
         return triggerEvent(newHandler, event, target);
      }

      state = context.getState();
      props = context.getProps();

      Term goal;
      VariableTerm newState = new VariableTerm("NewState");
      if (handler instanceof AtomTerm)
	 goal = Module.crossModuleCall(context.getComponentName(), new CompoundTerm((AtomTerm)handler, new Term[]{event, state, props, newState}));
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
         goal = Module.crossModuleCall(context.getComponentName(), new CompoundTerm(c_handler.tag.functor, args));
      }
      else
      {
         System.out.println("Handler is not callable: " + handler);
         return false;
      }
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
            // We must call getState() here since the state might have changed since we started! A series of events might have fired (specifically, Flux)
            // which might have mutated the state from its value when we started this event
            context.setState(applyState(context.getState(), newState.dereference()));
            interpreter.undo(undoPosition);
            return (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST);
	 }
//	 else
//	    System.out.println("Failed")
      }
      catch (PrologException notDefined)
      {
         notDefined.printStackTrace();
      }
      return false;
      // State is not updated if we get to here
   }

   public boolean triggerTest(Term handler, Term event, ReactWidget context) throws PrologException
   {
      PrologState state;
      PrologState props;
      if (handler instanceof CompoundTerm && ((CompoundTerm)handler).tag == tagThis)
      {
         CompoundTerm handlerTerm = (CompoundTerm)handler;
         ReactWidget target = (ReactWidget)((JavaObjectTerm)(handlerTerm.args[0])).value;
         Term newHandler = handlerTerm.args[1];
         return triggerTest(newHandler, event, target);
      }

      state = context.getState();
      props = context.getProps();

      Term goal;
      if (handler instanceof AtomTerm)
         goal = Module.crossModuleCall(context.getComponentName(), new CompoundTerm((AtomTerm)handler, new Term[]{event, state, props}));
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
         goal = Module.crossModuleCall(context.getComponentName(), new CompoundTerm(c_handler.tag.functor, args));
      }
      else
      {
         System.out.println("Handler is not callable: " + handler);
         return false;
      }
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

   public void waitForFluxStores(List<String> stores) throws PrologException
   {
      fluxDispatcher.waitFor(stores);
   }

   public void queueFluxEvent(Term key, Term value) throws PrologException
   {
      fluxDispatcher.queueEvent(key, value);
      fluxDispatcher.dispatchEvents();
   }

   public void deregisterFluxListener(String componentName, ReactWidget listener)
   {
      fluxDispatcher.deregisterFluxListener(componentName, listener);
   }

   public boolean updateStore(String componentName, Term key, Term value, PrologState state, FluxStore store)
   {
      Term goal;
      VariableTerm newState = new VariableTerm("NewState");
      goal = Module.crossModuleCall(componentName, new CompoundTerm(FluxDispatcher.handlerTag, new Term[]{key, value, state, newState}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      //System.out.println("Executing " + g);
      try
      {
         PrologCode.RC rc = interpreter.execute(g);
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            store.setState(applyState(state, newState.dereference()));
            interpreter.undo(undoPosition);
            return (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST);
         }
      }
      catch (PrologException notDefined)
      {
         notDefined.printStackTrace();
      }
      return false;
   }

   private PrologState applyState(PrologState oldState, Term newState) throws PrologException
   {
      return oldState.cloneWith(newState);
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

   public void setRootWidget(ReactWidget w)
   {
      rootWidget = w;
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
      public ExecutionState(URI uri, Term t, Environment e, Map<String, String> httpHeaders) throws IOException, InterruptedException
      {
         super(uri, new Draft_17(), httpHeaders);
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
	 else if (reply instanceof AtomTerm && ((AtomTerm)reply).value.equals("$aborted"))
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
	    try
	    {
	       replies.put(TermReader.stringToTerm(options, message, environment));
	    }
	    catch(ParseException e)
	    {
	       e.printStackTrace();
	       replies.put(new CompoundTerm(AtomTerm.get("exception"), new Term[]{new JavaObjectTerm(e)}));
	    }
	 }
	 catch(InterruptedException interrupted)
	 {
	    // If this happens I guess the result is just lost
	    interrupted.printStackTrace();
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
	 try
	 {
	    replies.put(AtomTerm.get("$aborted"));
	 }
	 catch(Exception e)
	 {
	    e.printStackTrace();
	 }
      }
      
      @Override
      public void onError(Exception ex)
      {
         ex.printStackTrace();
      }      
   }
   
   public ExecutionState prepareGoal(Term t, Environment e) throws IOException, InterruptedException
   {
      if (httpContext != null)
         return new ExecutionState(goalURI, t, e, httpContext.getHTTPHeaders());
      else
         return new ExecutionState(goalURI, t, e, emptyHTTPHeaders);
   }


   public Term diff(Term a, Term b) throws PrologException
   {
      VariableTerm patchTerm = new VariableTerm("Patch");
      Term goal = Module.crossModuleCall("vdiff", new CompoundTerm(AtomTerm.get("vdiff"), new Term[]{a, b, patchTerm}));
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
      Term goal = Module.crossModuleCall("vdiff", new CompoundTerm(AtomTerm.get("vpatch"), new Term[]{new JavaObjectTerm(root),
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
      //System.exit(-1);
      return null;
   }

   public Term render(ReactWidget widget, String component, PrologState state, PrologState props) throws PrologException
   {
      //System.out.println("Rendering " + component + " with props " + props + " and state " + state);
      VariableTerm vDom = new VariableTerm("VDom");
      Term goal = Module.crossModuleCall(component, new CompoundTerm(AtomTerm.get("render"), new Term[]{state,
													     props,
                                                                                                             vDom}));
      int undoPosition = interpreter.getUndoPosition();
      Interpreter.Goal g = interpreter.prepareGoal(goal);
      env.pushContext(interpreter, widget);
      PrologCode.RC rc;
      try
      {
         //System.out.println("Rendering: " + g);
         rc = interpreter.execute(g);
         //System.out.println("Rendering complete");
         if (rc == PrologCode.RC.SUCCESS)
            interpreter.stop(g);
         if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
         {
            VariableTerm expanded = new VariableTerm("expanded");
            Interpreter.Goal g2 = interpreter.prepareGoal(new CompoundTerm(AtomTerm.get("expand_children"), new Term[]{vDom.dereference(), expanded}));
            rc = interpreter.execute(g2);
            if (rc == PrologCode.RC.SUCCESS || rc == PrologCode.RC.SUCCESS_LAST)
            {
               Term result = expanded.dereference();
               result = result.clone(new TermCloneContext());
               interpreter.undo(undoPosition);
               return result;
            }
            System.out.println("expand_children/2 failed?");
         }
         else
            System.out.println("Render failed");
      }
      finally
      {
         env.popContext(interpreter);
      }
      System.out.println("Render returned null");
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


   
   public void registerWidget(ReactWidget w)
   {
      if (knownWidgets.get(w.getComponentName()) == null)
         knownWidgets.put(w.getComponentName(), new Vector<ReactWidget>());
      knownWidgets.get(w.getComponentName()).add(w);
   }

   public void deRegisterWidget(ReactWidget w)
   {
      knownWidgets.get(w.getComponentName()).remove(w);
   }

   public class ServerConnection extends WebSocketClient
   {
      public ServerConnection(URI uri)
      {
         super(uri);
         connect();
      }

      @Override
      public void onMessage(String message)
      {
         try
         {
            Term t = gnu.prolog.io.TermReader.stringToTerm((String)message, env);
            if (t instanceof CompoundTerm && ((CompoundTerm)t).tag == systemFunctor)
            {
               t = ((CompoundTerm)t).args[0];
               if (t instanceof CompoundTerm && ((CompoundTerm)t).tag == consultedFunctor)
               {
                  make();
                  rootWidget.reRender();
               }
               else
               {
                  // FIXME: Pass message up via API
               }
            }
            else if (t instanceof CompoundTerm && ((CompoundTerm)t).tag == messageFunctor)
            {
               Term module = ((CompoundTerm)t).args[0];
               Term handler = ((CompoundTerm)t).args[1];
               Term event = ((CompoundTerm)t).args[2];
               List<ReactWidget> widgets = knownWidgets.get(((AtomTerm)module).value);
               if (widgets != null)
               {
                  for (ReactWidget w : widgets)
                     w.triggerEvent(handler, event);
               }
            }
         }
         catch(Exception e)
         {
            e.printStackTrace();
         }
      }

      @Override
      public void onOpen(ServerHandshake handshake)
      {
         send(gnu.prolog.io.TermWriter.toString(AtomTerm.get(rootElementId)) + ".\n");
      }

      @Override
      public void onClose(int code, String reason, boolean remote)
      {

      }

      @Override
      public void onError(Exception error)
      {
         error.printStackTrace();
         serverConnection = new ServerConnection(listenURI);
      }
   }

   public void sendAsyncMessage(Term t)
   {
      serverConnection.send("message(" + gnu.prolog.io.TermWriter.toString(t) + ").\n");
   }

   public void registerForMessages(Term t)
   {
      System.out.println("listen_for(" + gnu.prolog.io.TermWriter.toString(t) + ").\n");
      serverConnection.send("listen_for(" + gnu.prolog.io.TermWriter.toString(t) + ").\n");
   }


}
