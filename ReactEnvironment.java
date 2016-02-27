import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;
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
      ((ReactLoaderState)prologTextLoaderState).setModule(userModule);
      this.engine = engine;
   }
   
   public Engine getEngine()
   {
      return engine;
   }

   public void pushModule(String moduleName)
   {
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
      System.out.println("Module: " + name + " with exports " + exports);
      ReactModule newModule = new ReactModule(name, exports);
      modules.put(name, newModule);
      moduleStack.push(name);
      try
      {
         // Enable us to get out later!
         installBuiltin("with_module", 2);
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
      {
         System.out.println("Loading :/2");
         p.setJavaClassName("Predicate_colon");
      }
      else
         p.setJavaClassName("Predicate_" + functor);
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
