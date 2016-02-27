import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;

public class ReactLoaderState extends PrologTextLoaderState
{
   protected ReactModule module = new ReactUserModule();
   private final Object currentPredicateLock = new Object();
   
   public ReactLoaderState(ReactEnvironment env)
   {      
      super(env);
   }


   public void setModule(ReactModule module)
   {
      this.module = module;
   }
   
   @Override
   public void addInitialization(PrologTextLoader loader, Term term)
   {
      try
      {
         ReactEnvironment environment = (ReactEnvironment)getEnvironment();
         if (term instanceof CompoundTerm && ((CompoundTerm)term).tag.arity == 2 && ((CompoundTerm)term).tag.functor.value.equals("module"))
         {
            CompoundTerm directive = (CompoundTerm)term;
            Term[] args = directive.args;
            AtomTerm moduleName = (AtomTerm)args[0];
            List<CompoundTermTag> exports = new LinkedList<CompoundTermTag>();
            if (args[1] instanceof CompoundTerm)
            {
               CompoundTerm list = (CompoundTerm)args[1];
               if (list.tag == TermConstants.listTag)
               {
                  while(list.tag.arity == 2)
                  {
                     if (list.args[0] instanceof CompoundTerm &&
                         ((CompoundTerm)list.args[0]).tag.arity == 2 &&
                         ((CompoundTerm)list.args[0]).tag.functor.value.equals("/"))
                     {
                        CompoundTerm head = (CompoundTerm)list.args[0];
                        if (head.args[0] instanceof AtomTerm && head.args[1] instanceof IntegerTerm)
                           exports.add(CompoundTermTag.get(head));
                        else
                           PrologException.typeError(AtomTerm.get("predicate_indicator"), head);
                  
                        if (list.args[1] instanceof CompoundTerm)
                           list = (CompoundTerm)list.args[1];
                        else if (TermConstants.emptyListAtom.equals(list.args[1]))
                           break;
                        else
                           PrologException.typeError(AtomTerm.get("list"), args[1]);
                     }
                     else
                        PrologException.typeError(AtomTerm.get("predicate_indicator"), list.args[0]);
                  }
               }
            }
            else if (!TermConstants.emptyListAtom.equals(args[1]))
               PrologException.typeError(AtomTerm.get("list"), args[1]);
            // Switch to a brand new module here
            module = environment.startNewModule(moduleName.value, exports);
         }
         else
            module.addInitialization(loader.getCurrentPartialLoaderError(), term);
      }
      catch(PrologException e)
      {
         e.printStackTrace();
      }
   }

   // Yuck :(
   @Override
   public ReactModule getModule()
   {
      return module;
   }

   @Override
   public boolean declareDynamic(PrologTextLoader loader, CompoundTermTag tag)
   {
      Predicate p = module.getOrCreateDefinedPredicate(tag);
      if (testOption(loader, p, "dynamic"))
      {
         return true;
      }
      if (isDeclaredInOtherLoaders(loader, p))
      {
         if (!testOption(loader, p, "multifile"))
         {
            logError(loader, "non multifile predicate could not be changed in other prolog text.");
            return false;
         }
         if (!testOption(null, p, "dynamic"))
         {
            logError(loader,
                     "predicate was not declared dynamic in other texts, dynamic option should be the same in each prolog text.");
            return false;
         }
      }
      else
      {
         if (testOption(loader, p, "defined"))
         {
            logError(loader, "predicate was already defined and could not be declared dynamic.");
            return false;
         }
      }
      if (p.getType() == Predicate.TYPE.UNDEFINED)
      {
         p.setType(Predicate.TYPE.USER_DEFINED);
      }
      p.setDynamic();
      defineOptionAndDeclare(loader, p, "dynamic");
      return true;
   }

   @Override
   public void declareMultifile(PrologTextLoader loader, CompoundTermTag tag)
   {
      Predicate p = module.getOrCreateDefinedPredicate(tag);
      if (testOption(loader, p, "multifile"))
      {
         return;
      }
      if (isDeclaredInOtherLoaders(loader, p))
      {
         if (!testOption(null, p, "multifile"))
         {
            logError(loader, "non multifile predicate could not be changed in other prolog text.");
            return;
         }
      }
      else
      {
         if (testOption(loader, p, "defined"))
         {
            logError(loader, "predicate was already defined and could not be declared multifile.");
            return;
         }
      }
      if (p.getType() == Predicate.TYPE.UNDEFINED)
      {
         p.setType(Predicate.TYPE.USER_DEFINED);
      }
      defineOptionAndDeclare(loader, p, "multifile");
   }

   @Override
   public void declareDiscontiguous(PrologTextLoader loader, CompoundTermTag tag)
   {
      Predicate p = module.getOrCreateDefinedPredicate(tag);
      if (testOption(loader, p, "discontiguous"))
      {
         return;
      }
      if (isDeclaredInOtherLoaders(loader, p))
      {
         if (!testOption(null, p, "multifile"))
         {
            logError(loader, "non multifile predicate could not be changed in other prolog text.");
            return;
         }
      }
      if (testOption(loader, p, "defined"))
      {
         logError(loader, "predicate was already defined and could not be declared discontiguous.");
         return;
      }
      if (p.getType() == Predicate.TYPE.UNDEFINED)
      {
         p.setType(Predicate.TYPE.USER_DEFINED);
      }
      defineOptionAndDeclare(loader, p, "discontiguous");
   }

   @Override
   public void addClause(PrologTextLoader loader, Term term)
   {
      if (!(module instanceof ReactUserModule))
         System.out.println("-->" + module + ":" +term);
      Term head = term;
      CompoundTermTag headTag;
      if (term instanceof CompoundTerm && ((CompoundTerm) term).tag == TermConstants.clauseTag)
      {
         head = ((CompoundTerm) term).args[0];
      }
      if (head instanceof AtomTerm)
      {
         headTag = CompoundTermTag.get((AtomTerm) head, 0);
      }
      else if (head instanceof CompoundTerm)
      {
         headTag = ((CompoundTerm) head).tag;
      }
      else
      {
         logError(loader, "predicate head is not a callable term.");
         return;
      }
      synchronized (currentPredicateLock)
      {
         if (currentPredicate == null || headTag != currentPredicate.getTag())
         {
            currentPredicate = null;
            Predicate p = module.getOrCreateDefinedPredicate(headTag);
            if (testOption(loader, p, "defined") && !testOption(loader, p, "discontiguous"))
            {
               logError(loader, "predicate is not discontiguous.");
               return;
            }
            if (!testOption(loader, p, "declared") && testOption(null, p, "declared")
                && !testOption(loader, p, "multifile"))
            {
               logError(loader, "predicate is not multifile: " + p.getTag());
               return;
            }
            if (!testOption(loader, p, "dynamic") && testOption(null, p, "dynamic"))
            {
               logError(loader, "predicate is not declared dynamic in this prolog text.");
               return;
            }
            currentPredicate = p;
            if (!testOption(loader, p, "defined"))
            {
               if (p.getType() == Predicate.TYPE.UNDEFINED)
               {
                  p.setType(Predicate.TYPE.USER_DEFINED);
               }
               defineOptionAndDeclare(loader, p, "defined");
            }
         }
         try
         {
            currentPredicate.addClauseLast(Predicate.prepareClause(term));
         }
         catch (PrologException ex)
         {
            logError(loader, ex.getMessage());
         }
      }
   }

   @Override
   public void defineExternal(PrologTextLoader loader, CompoundTerm pi, String javaClassName, Predicate.TYPE type)
   {
      if (!CompoundTermTag.isPredicateIndicator(pi))
      {
         logError(loader, "predicate indicator is not valid.");
         return;
      }
      CompoundTermTag tag = CompoundTermTag.get(pi);
      Predicate p = module.getOrCreateDefinedPredicate(tag);
      if (p.getType() != Predicate.TYPE.UNDEFINED)
      {
         logError(loader, "predicate type could not be changed.");
         return;
      }
      p.setType(type);
      p.setJavaClassName(javaClassName);
      defineOptionAndDeclare(loader, p, "defined");
   }
   
}
