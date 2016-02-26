import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;

public class ReactEnvironment extends Environment
{
   private Engine engine;
   public String currentModule = "user";
   private Map<String, ReactModule> modules;
   public ReactEnvironment(Engine engine)
   {
      modules = new HashMap<String, ReactModule>();
      // In reality, EVERYTHING in user is exported...
      ReactModule userModule = new ReactModule("user", new LinkedList<CompoundTermTag>());
      modules.put("user", userModule);
      this.engine = engine;
   }
   
   public Engine getEngine()
   {
      return engine;
   }

   @Override
   public ReactModule getModule()
   {
      System.out.println("Returning module for " + currentModule + ": " + modules.get("user"));
      return modules.get(currentModule);
   }

   @Override
   public void createTextLoader()
   {
      prologTextLoaderState = new ReactLoaderState(this);
   }

   public synchronized void ensureLoaded(String baseURI, String component)
   {      
      prologTextLoaderState.ensureLoaded(new CompoundTerm(CompoundTermTag.get("url", 1), AtomTerm.get(baseURI + component)));
      // Now find solutions to require() and recursively load them too? In reality, we can load a single file and declare many modules inside it
   }

   public ReactModule startNewModule(String name, List<CompoundTermTag> exports)
   {
      System.out.println("Module: " + name + " with exports " + exports);
      ReactModule newModule = new ReactModule(name, exports);
      modules.put(name, newModule);
      currentModule = name;
      return newModule;
   }

   public void linkModules()
   {
      for (Map.Entry<String, ReactModule> exporter : modules.entrySet())
      {
         for (Map.Entry<String, ReactModule> importer : modules.entrySet())
         {
            if (exporter.getKey().equals(importer.getKey()))
               continue;
            importer.getValue().importPredicates(exporter.getValue());
         }
      }
   }
}
