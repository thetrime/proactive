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

public class FluxDispatcher
{
   public static final CompoundTermTag handlerTag = CompoundTermTag.get("handle_event", 5);
   private static Map<String, LinkedList<PrologContext>> eventListeners = new HashMap<String, LinkedList<PrologContext>>();

   private static boolean isProcessing = false;
   private static Queue<String> unprocessed = null;
   private static Queue<String> processed = null;
   private static Term currentKey = null;
   private static Term currentValue = null;

   private static List<String> listenerModules = new LinkedList<String>();

   public static void registerHandlerModule(String name)
   {
      listenerModules.add(name);
   }

   public static void registerFluxListener(String componentName, PrologContext context)
   {
      System.out.println("Checking " + componentName + " for fluxion");
      if (!listenerModules.contains(componentName))
         return;
      System.out.println("fluxion located. Linking...");
      LinkedList<PrologContext> existing = eventListeners.get(componentName);
      if (existing == null)
      {
         existing = new LinkedList<PrologContext>();
         eventListeners.put(componentName, existing);
      }
      existing.add(context);
   }

   // Does this happen every time a component is destroyed, or only if a module gets unloaded somehow?
   // The critical thing is: If we have created a new PrologContext, we must need a new listener...
   // In particular one module can result in TWO listeners if it is used twice, like <Foo><Bar x=1/><Bar x=2/></Foo>
   public static void deregisterFluxListener(String componentName, PrologContext context)
   {
      List<PrologContext> existing = eventListeners.get(componentName);
      if (existing != null)         
         existing.remove(context);
   }
   
   public static synchronized void queueEvent(Term key, Term value) throws PrologException
   {
      if (isProcessing)
         PrologException.permissionError(AtomTerm.get("raise"), AtomTerm.get("event"), AtomTerm.get("<while processing an existing event>"));
      isProcessing = true;
      currentKey = key;
      currentValue = value;
      unprocessed = new LinkedList<String>();
      processed = new LinkedList<String>();
      unprocessed.addAll(eventListeners.keySet());
   }

   public static void dispatchEvents()
   {
      System.out.println("Dispatching flux events. " + unprocessed.size() + " modules pending");
      while (unprocessed.size() > 0)
      {
         String componentName = unprocessed.remove();
         LinkedList<PrologContext> tasks = new LinkedList<PrologContext>();
         tasks.addAll(eventListeners.get(componentName));
         PrologContext context;
         while((context = tasks.poll()) != null)
         {
            System.out.println(" Dispatching an event. " + tasks.size() + " tasks remaining in " + componentName);
            try
            {
               context.fluxEvent(currentKey, currentValue);
            }
            catch(Exception e)
            {
               e.printStackTrace();
            }
            System.out.println(" Dispatched an event. Remaining: " + tasks.size());
         }
         System.out.println("Flux finished dispatching events to instances of " + componentName);
         processed.add(componentName);
      }
      isProcessing = false;
   }

   public static void waitFor(List<String> tokens)
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
