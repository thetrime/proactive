package org.proactive.prolog;

import gnu.prolog.database.Predicate;
import java.util.LinkedList;
import gnu.prolog.term.CompoundTermTag;

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
