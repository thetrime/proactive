package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.JavaObjectTerm;
import org.proactive.ReactComponent;

public class Predicate_set_properties extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactComponent domNode = (ReactComponent)((JavaObjectTerm)args[0]).value;
      HashMap<String, PrologObject> properties = new HashMap<String,PrologObject>();
      if (!TermConstants.emptyListAtom.equals(args[1]))
      {
         if (args[1] instanceof CompoundTerm)
         {
            CompoundTerm list = (CompoundTerm)args[1];
            while (list.tag == TermConstants.listTag && list.arity == 2)
            {
               if (list.args[0] instanceof CompoundTerm)
               {
                  CompoundTerm attr = (CompoundTerm)list.args[0];
                  if (attr.tag.arity != 2 || !attr.tag.functor.value.equals("="))
                     PrologException.typeError(AtomTerm.get("attribute"), attr);
                  Term attrName = attr.args[0];
                  Term attrValue = attr.args[1];
                  if (attrName instanceof AtomTerm)
                     properties.put(((AtomTerm)attrName).value, new PrologObject(attrValue));
                  else
                     PrologException.typeError(AtomTerm.get("atom"), attrName);
                  if (TermConstants.emptyListAtom.equals(list.args[1]))
                     break;
                  else if (list.args[1] instanceof CompoundTerm)
                     list = (CompoundTerm)list.args[1];
                  else
                     PrologException.typeError(AtomTerm.get("list"), args[1]);
               }
            }
         }
      }
      String name = ((AtomTerm)args[1]).value;
      domNode.setProperties(properties);
      return RC.SUCCESS_LAST;
   }
}
