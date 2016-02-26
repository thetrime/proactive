import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;

public class ReactModule extends Module
{
   public List<CompoundTermTag> exports;
   String name = "user";

   public ReactModule(String name, List<CompoundTermTag> exports)
   {
      this.name = name;
      this.exports = exports;
   }
   public String toString()
   {
      return name;
   }
   
   public void importPredicates(ReactModule exportModule)
   {
      for (CompoundTermTag export : exportModule.exports)
      {
         System.out.println("Trying to create predicate " + export + " in module " + this);
         // First, we have to change the current module
         //String oldModule = currentModule;
         try
         {
            //currentModule = name;
            Predicate p = createDefinedPredicate(export);
            Term head = new CompoundTerm(export);
            Term body = new CompoundTerm(CompoundTermTag.get("with_module", 2), new Term[]{AtomTerm.get(name), head});
            Term linkClause = new CompoundTerm(CompoundTermTag.get(":-", 2), new Term[]{head, body});
            p.setType(Predicate.TYPE.USER_DEFINED);
            p.addClauseLast(linkClause);
         }
         finally
         {
            //currentModule = oldModule;
         }
      }
   }
}
