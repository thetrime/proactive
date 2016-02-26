import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;

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
         Predicate p = createDefinedPredicate(export);
         Term[] args = new Term[export.arity];
         for (int i = 0; i < args.length; i++)
            args[i] = new VariableTerm();
         Term head = new CompoundTerm(export, args);
         Term body = new CompoundTerm(CompoundTermTag.get("with_module", 2), new Term[]{AtomTerm.get(exportModule.name), head});
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
}
