import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;

public class ReactUserModule extends ReactModule
{
   public ReactUserModule()
   {
      super("user", new LinkedList<CompoundTermTag>());
   }

   @Override
   public Predicate getOrCreateDefinedPredicate(CompoundTermTag headTag)
   {
      if (!exports.contains(headTag))
         exports.add(headTag);
      return super.getOrCreateDefinedPredicate(headTag);
   }

   @Override
   public Predicate createDefinedPredicate(CompoundTermTag tag)
   {
      if (!exports.contains(tag))
         exports.add(tag);
      return super.createDefinedPredicate(tag);
   }
}
