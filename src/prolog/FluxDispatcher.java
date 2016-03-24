package org.proactive.prolog;

import gnu.prolog.term.Term;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.CompoundTermTag;
import gnu.prolog.vm.PrologException;
import java.util.Queue;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import org.proactive.ReactWidget;
import org.proactive.prolog.Engine;

public class FluxDispatcher
{
   public static final CompoundTermTag handlerTag = CompoundTermTag.get("handle_event", 4);
   private HashMap<String, FluxStore> stores = new HashMap<String, FluxStore>();

   private boolean isProcessing = false;
   private Queue<String> unprocessed = null;
   private Queue<String> processed = null;
   private Term currentKey = null;
   private Term currentValue = null;
   private Engine engine;

   public PrologState getStoreState(String name)
   {
      FluxStore store = stores.get(name);
      if (store == null)
	 return PrologState.emptyState;
      return store.getState();
   }

   public void initializeFlux(Engine engine)
   {
      this.engine = engine;
      for (Map.Entry<String, FluxStore> entry : stores.entrySet())
         entry.getValue().initialize(engine);
   }

   public void registerFluxListener(String storeName, Term callback, ReactWidget context)
   {
      //System.out.println("Checking " + componentName + " for fluxion");
      if (!stores.containsKey(storeName))
      {
         // This happens if we register an interest in a module before we actually hear about the module itself
         // Do not initialize it yet though
         stores.put(storeName, new FluxStore(storeName, engine));
      }
      stores.get(storeName).addListener(context, callback);
   }

   public void deregisterFluxListener(String componentName, ReactWidget context)
   {
      if (stores.containsKey(componentName))
         stores.get(componentName).removeListener(context);
      else
         System.out.println("Warning: Attempt to deregister " + componentName + " which is not currently listening to anything...");
   }
   
   public synchronized void queueEvent(Term key, Term value) throws PrologException
   {
      if (isProcessing)
         PrologException.permissionError(AtomTerm.get("raise"), AtomTerm.get("event"), AtomTerm.get("<while processing an existing event>"));
      isProcessing = true;
      currentKey = key;
      currentValue = value;
      unprocessed = new LinkedList<String>();
      processed = new LinkedList<String>();
      unprocessed.addAll(stores.keySet());
   }

   public void dispatchEvents()
   {
      //System.out.println("Dispatching flux events. " + unprocessed.size() + " modules pending");
      System.out.println("Dispatching flux events. Pending: " + unprocessed);
      while (unprocessed.size() > 0)
      {
         String componentName = unprocessed.remove();
         // Dispatch the event to the store
         FluxStore store = stores.get(componentName);
         store.handleEvent(currentKey, currentValue);
         System.out.println("Flux finished dispatching events to instances of " + componentName);
         processed.add(componentName);
      }
      isProcessing = false;
   }

   public void waitFor(List<String> tokens)
   {
      if (isProcessing == false)
         throw new RuntimeException("FIXME: Surely this is impossible?");
      LinkedList<String> stillWaiting = null;
      do
      {         
         stillWaiting = new LinkedList<String>();
         for (String token : tokens)
         {
            if (unprocessed.contains(token))
            {
               stillWaiting.add(token);
            }
            else if (!processed.contains(token))
            {
               throw new RuntimeException("FIXME: This is a dependency loop, I think");
            }
         }
         dispatchEvents();
      } while(stillWaiting.size() > 0);
   }
}
