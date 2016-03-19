package org.proactive.prolog;

import gnu.prolog.term.Term;
import org.proactive.ReactWidget;
import org.proactive.prolog.Engine;
import java.util.LinkedList;

public class FluxStore
{
   protected Term state;
   protected String storeName;
   protected Engine engine;
   private LinkedList<FluxListener> listeners = new LinkedList<FluxListener>();

   public FluxStore(String storeName, Engine engine)
   {
      this.storeName = storeName;
      if (engine != null)
         initialize(engine);
   }

   public void initialize(Engine engine)
   {
      this.engine = engine;
      state = engine.getInitialStoreState(storeName);
   }

   public void addListener(ReactWidget listener, Term callback)
   {
      listeners.add(new FluxListener(listener, callback));
   }

   public void removeListener(ReactWidget listener)
   {
      listeners.remove(listener);
   }

   public boolean handleEvent(Term key, Term value)
   {
      System.out.println("Updating store " + storeName + " engine=" + engine);
      return engine.updateStore(storeName, key, value, state, this);
   }

   public void setState(Term t)
   {
      state = t;
      for (FluxListener listener: listeners)
      {
         try
         {
            listener.context.fluxEvent(listener.callback, storeName, state);
         }
         catch(Exception e)
         {
            e.printStackTrace();
         }
      }
   }

   public Term getState()
   {
      return state;
   }

   private class FluxListener
   {
      public Term callback;
      public ReactWidget context;
      public FluxListener(ReactWidget context, Term callback)
      {
         this.context = context;
         this.callback = callback;
      }
   }

}
