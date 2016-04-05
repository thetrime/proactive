package org.proactive.prolog;

import gnu.prolog.database.PredicateUpdatedEvent;
import gnu.prolog.database.Predicate;
import gnu.prolog.database.Module;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.CompoundTermTag;
import gnu.prolog.vm.Environment;
import gnu.prolog.vm.PrologCodeListener;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.PrologCode;
import gnu.prolog.vm.Interpreter;
import java.util.Stack;
import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.lang.ref.WeakReference;
import java.lang.ref.ReferenceQueue;

public class ReactEnvironment extends Environment
{
   private Engine engine;
   private HashMap<Interpreter, Stack<Object>> contextStack = new HashMap<Interpreter, Stack<Object>>();
   public ReactEnvironment(Engine engine)
   {
      super();
      this.engine = engine;
   }

   public void pushContext(Interpreter i, Object o)
   {
      Stack<Object> stack = contextStack.get(i);
      if (stack == null)
      {
         stack = new Stack<Object>();
         contextStack.put(i, stack);
      }
      stack.push(o);
   }

   public void popContext(Interpreter i)
   {
      Stack<Object> stack = contextStack.get(i);
      if (stack != null)
         stack.pop();
   }

   public Object getCurrentContext(Interpreter i)
   {
      Stack<Object> stack = contextStack.get(i);
      if (stack != null)
         return stack.peek();
      // This should be a warning!
      return null;

   }

   public Engine getEngine()
   {
      return engine;
   }

   public synchronized void ensureLoaded(String baseURI, String component)
   {
      prologTextLoaderState.ensureLoaded(new CompoundTerm(CompoundTermTag.get("url", 1), AtomTerm.get(baseURI + component)));
   }

   public boolean predicateExists(String moduleName, CompoundTermTag tag)
   {
      try
      {
         pushModule(AtomTerm.get(moduleName));
      }
      catch(PrologException noSuchModule)
      {
         return false;
      }
      try
      {
         Module m = getModule();
         return m.getDefinedPredicate(tag) != null;
      }
      finally
      {
         popModule();
      }
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
}
