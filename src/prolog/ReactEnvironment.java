package org.proactive.prolog;

import gnu.prolog.database.PredicateUpdatedEvent;
import gnu.prolog.database.Predicate;
import gnu.prolog.database.Module;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.CompoundTermTag;
import gnu.prolog.vm.Environment;
import gnu.prolog.vm.PrologCodeListener;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.PrologCode;
import java.util.Stack;
import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.lang.ref.WeakReference;
import java.lang.ref.ReferenceQueue;

public class ReactEnvironment extends Environment
{
   private Engine engine;
   public Stack<String> moduleStack = new Stack<String>();
   private Map<String, ReactModule> modules;
   public ReactEnvironment(Engine engine)
   {
      modules = new HashMap<String, ReactModule>();
      // The user module exports everything by default
      ReactModule userModule = ((ReactLoaderState)prologTextLoaderState).getModule();
      modules.put("user", userModule);
      moduleStack.push("user");
      try
      {
         installBuiltin(":", 2);
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
      ((ReactLoaderState)prologTextLoaderState).setModule(userModule);
      this.engine = engine;
   }
   
   public Engine getEngine()
   {
      return engine;
   }

   public void pushModule(String moduleName) throws PrologException
   {
      if (modules.get(moduleName) == null)
         PrologException.existenceError(AtomTerm.get("module"), AtomTerm.get(moduleName));
      moduleStack.push(moduleName);
   }

   public void popModule()
   {
      moduleStack.pop();
   }

   @Override
   public ReactModule getModule()
   {
      return modules.get(moduleStack.peek());
   }

   @Override
   public void createTextLoader()
   {
      prologTextLoaderState = new ReactLoaderState(this);
   }

   public synchronized void ensureLoaded(String baseURI, String component)
   {
      prologTextLoaderState.ensureLoaded(new CompoundTerm(CompoundTermTag.get("url", 1), AtomTerm.get(baseURI + component)));
   }

   public ReactModule startNewModule(String name, List<CompoundTermTag> exports)
   {
      System.out.println("New module: " + name + " with exports " + exports);
      if (modules.get(name) != null)
      {
         System.out.println("Module " + name + " already exists!");
         System.exit(-1);
      }
      ReactModule newModule = new ReactModule(name, exports);
      modules.put(name, newModule);
      moduleStack.push(name);
      try
      {
         // Enable us to get out later!
         installBuiltin(":", 2);
      }
      catch(Exception surelyNot)
      {
         surelyNot.printStackTrace();
      }
      return newModule;
   }

   public void linkModules() throws PrologException
   {
      for (Map.Entry<String, ReactModule> exporter : modules.entrySet())
      {
         for (Map.Entry<String, ReactModule> importer : modules.entrySet())
         {
            if (exporter.getKey().equals(importer.getKey()))
               continue;
            if (importer.getKey().equals("user"))
               continue;
            System.out.println("================== importing " + exporter.getValue() + " into " + importer.getValue());            
            importer.getValue().importPredicates(exporter.getValue(), this);
         }
      }
      
      for (Map.Entry<String, ReactModule> exporter : modules.entrySet())
      {
         if (exporter.getKey().equals("user"))
             continue;
         ReactModule importer = modules.get("user");
         importer.importPredicates(exporter.getValue(), this);
      }
      // Ok, now we can go back to user
      moduleStack = new Stack<String>();
      moduleStack.push("user");
   }

   public void installBuiltin(String functor, int arity) throws PrologException
   {
      Module module = getModule();
      CompoundTermTag head = CompoundTermTag.get(AtomTerm.get(functor), arity);
      Predicate p = module.createDefinedPredicate(head);
      p.setType(Predicate.TYPE.BUILD_IN);
      if (functor.equals(":"))
         p.setJavaClassName("org.proactive.prolog.Predicate_colon");
      else
         p.setJavaClassName("org.proactive.prolog.Predicate_" + functor);
      PrologCode q = loadPrologCode(head);
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
   
   public void addPrologCodeListener(CompoundTermTag tag, PrologCodeListener listener)
   {
      getModule().addPrologCodeListener(this, tag, listener);
   }
  

   public void removePrologCodeListener(CompoundTermTag tag, PrologCodeListener listener)
   {
      getModule().removePrologCodeListener(this, tag, listener);
   }


   @Override
   public synchronized PrologCode getPrologCode(CompoundTermTag tag) throws PrologException
   {
      return getModule().getPrologCode(this, tag);
   }

   @Override
   public void predicateUpdated(PredicateUpdatedEvent evt)
   {
      getModule().predicateUpdated(this, evt);
   }
}
