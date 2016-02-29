package org.proactive.prolog;

import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;
import java.lang.ref.WeakReference;
import java.lang.ref.ReferenceQueue;

public class ReactModule extends Module
{
   public List<CompoundTermTag> exports;
   protected String name = null;

   public ReactModule(String name, List<CompoundTermTag> exports)
   {
      this.name = name;
      this.exports = exports;
   }
   public String toString()
   {
      return name;
   }
   
   public void importPredicates(ReactModule exportModule, ReactEnvironment environment) throws PrologException
   {
      if (exportModule instanceof ReactUserModule)
      {
         // basically just copy everything.
         for (CompoundTermTag export : exportModule.exports)
         {
            Predicate p = exportModule.getDefinedPredicate(export);
            tag2predicate.put(export, p);            
         }
         return;
      }
      for (CompoundTermTag export : exportModule.exports)
      {
         Predicate p;
         try
         {
            p = createDefinedPredicate(export);
         }
         catch(IllegalStateException e)
         {
            System.out.println("Error creating " + export + " in " + name);
            e.printStackTrace();
            continue;
         }
         Term[] args = new Term[export.arity];
         for (int i = 0; i < args.length; i++)
            args[i] = new VariableTerm();
         Term head = new CompoundTerm(export, args);
         Term body = crossModuleCall(exportModule.name, head);
         Term linkClause = new CompoundTerm(CompoundTermTag.get(":-", 2), new Term[]{head, body});
         p.setType(Predicate.TYPE.USER_DEFINED);
         p.addClauseLast(linkClause);
         try
         {
            environment.pushModule(exportModule.name);
            environment.loadPrologCode(export);
         }
         catch(Throwable builtin)
         {
            // FIXME: This is not good!
         }
         finally
         {
            environment.popModule();
         }
      }
      
   }

   public static Term crossModuleCall(String targetModule, Term goal)
   {
      return new CompoundTerm(AtomTerm.get(":"), new Term[]{AtomTerm.get(targetModule), goal});
   }


   protected Map<CompoundTermTag, PrologCode> tag2code = new HashMap<CompoundTermTag, PrologCode>();
   protected final Map<CompoundTermTag, List<PrologCodeListenerRef>> tag2listeners = new HashMap<CompoundTermTag, List<PrologCodeListenerRef>>();
   protected final ReferenceQueue<? super PrologCodeListener> prologCodeListenerReferenceQueue = new ReferenceQueue<PrologCodeListener>();

   public synchronized PrologCode getPrologCode(Environment environment, CompoundTermTag tag) throws PrologException
   {
      PrologCode code = tag2code.get(tag);
      if (code == null)
      {
         code = environment.loadPrologCode(tag);
         tag2code.put(tag, code);
      }
      return code;
   }

   public void predicateUpdated(Environment environment, PredicateUpdatedEvent evt)
   {
      PrologCode code = tag2code.remove(evt.getTag());
      pollPrologCodeListeners();
      if (code == null) // if code was not loaded yet
      {
         return;
      }
      CompoundTermTag tag = evt.getTag();
      synchronized (tag2listeners)
      {
         List<PrologCodeListenerRef> list = tag2listeners.get(tag);
         if (list != null)
         {
            PrologCodeUpdatedEvent uevt = new PrologCodeUpdatedEvent(environment, tag);
            ListIterator<PrologCodeListenerRef> i = list.listIterator();
            while (i.hasNext())
            {
               PrologCodeListenerRef ref = i.next();
               PrologCodeListener lst = ref.get();
               if (lst == null)
               {
                  i.remove();
               }
               else
               {
                  lst.prologCodeUpdated(uevt);
                  return;
               }
            }
         }
      }
   }

   protected void pollPrologCodeListeners()
   {
      PrologCodeListenerRef ref;
      synchronized (tag2listeners)
      {
         while (null != (ref = (PrologCodeListenerRef) prologCodeListenerReferenceQueue.poll()))
         {
            List<PrologCodeListenerRef> list = tag2listeners.get(ref.tag);
            list.remove(ref);
         }
      }
   }

   public void addPrologCodeListener(Environment environment, CompoundTermTag tag, PrologCodeListener listener)
   {
      synchronized (tag2listeners)
      {
         pollPrologCodeListeners();
         List<PrologCodeListenerRef> list = tag2listeners.get(tag);
         if (list == null)
         {
            list = new ArrayList<PrologCodeListenerRef>();
            tag2listeners.put(tag, list);
         }
         list.add(new PrologCodeListenerRef(prologCodeListenerReferenceQueue, listener, tag));
      }
   }

   public void removePrologCodeListener(Environment environment, CompoundTermTag tag, PrologCodeListener listener)
   {
      synchronized (tag2listeners)
      {
         pollPrologCodeListeners();
         List<PrologCodeListenerRef> list = tag2listeners.get(tag);
         if (list != null)
         {
            ListIterator<PrologCodeListenerRef> i = list.listIterator();
            while (i.hasNext())
            {
               PrologCodeListenerRef ref = i.next();
               PrologCodeListener lst = ref.get();
               if (lst == null)
               {
                  i.remove();
               }
               else if (lst == listener)
               {
                  i.remove();
                  return;
               }
            }
         }
      }
   }


   private static class PrologCodeListenerRef extends WeakReference<PrologCodeListener>
   {
      PrologCodeListenerRef(ReferenceQueue<? super PrologCodeListener> queue, PrologCodeListener listener,
                            CompoundTermTag tag)
      {
         super(listener, queue);
         this.tag = tag;
      }

      CompoundTermTag tag;
   }

   public Predicate getOrCreateDefinedPredicate(CompoundTermTag head)
   {
      if (!(this instanceof ReactUserModule))
      {
         if (head.equals(FluxDispatcher.handlerTag))
            FluxDispatcher.registerHandlerModule(name);
      }
      return super.getOrCreateDefinedPredicate(head);
   }
   
}
