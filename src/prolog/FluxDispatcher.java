package org.proactive.prolog;

import gnu.prolog.term.Term;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.vm.PrologException;
import java.util.Queue;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

public class FluxDispatcher
{
   private static Map<String, List<PrologContext>> eventListeners = new HashMap<String, List<PrologContext>>();

   private static boolean isProcessing = false;
   private static Queue<String> unprocessed = null;
   private static Queue<String> processed = null;
   private static Term currentKey = null;
   private static Term currentValue = null;

   public static void registerFluxListener(String componentName, PrologContext context)
   {
      List<PrologContext> existing = eventListeners.get(componentName);
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
      while (unprocessed.size() > 0)
      {
         String componentName = unprocessed.remove();
         List<PrologContext> tasks = eventListeners.get(componentName);
         for (PrologContext context : tasks)
         {
            try
            {
               context.fluxEvent(currentKey, currentValue);
            }
            catch(Exception e)
            {
               e.printStackTrace();
            }
         }
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
