import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.util.*;

public class PrologElement extends PrologNode
{
   public PrologElement()
   {
      nodeName = "<special-root>";
   }

   public String toString()
   {
      return "element(" + nodeName + "....)";
   }
   
   public PrologElement(Term element) throws Exception
   {
      if (!(element instanceof CompoundTerm))
         throw new RuntimeException("Invalid XML tree: element is not compound:" +element);
      CompoundTerm c = (CompoundTerm)element;      
      if (!(c.tag.functor.value.equals("element") && c.tag.arity == 3))
         throw new RuntimeException("Invalid XML tree: element is not element/3");
      // Ok, it is a valid element/3 term. Extract the name
      Term arg1 = c.args[0];
      Term arg2 = c.args[1];
      Term arg3 = c.args[2];
      if (arg1 instanceof AtomTerm)
         nodeName = ((AtomTerm)arg1).value;
      else
         throw new RuntimeException("Invalid XML tree: element tag is not an atom: " + arg1);
      if (!TermConstants.emptyListAtom.equals(arg2))
      {
         if (!(arg2 instanceof CompoundTerm))
            throw new RuntimeException("Invalid XML tree: attributes is not a compound: " + arg2);
         CompoundTerm childTerm = (CompoundTerm)arg2;
         if (childTerm.tag == TermConstants.listTag)
         {
            while(childTerm.tag.arity == 2)
            {
               if (childTerm.args[0] instanceof CompoundTerm)
               {
                  CompoundTerm attr = (CompoundTerm)childTerm.args[0];
                  if (attr.tag.arity != 2 || !attr.tag.functor.value.equals("="))                     
                     throw new RuntimeException("Invalid XML tree: Attribute is not =/2: " + attr);
                  Term attrName = attr.args[0];
                  Term attrValue = attr.args[1];
                  if (!(attrName instanceof AtomTerm))
                     throw new RuntimeException("Invalid XML tree: Attribute name is not an atom: " + attrName);
                  attributes.put(((AtomTerm)attrName).value, attrValue.dereference());
               }               
               else
               {
                  throw new RuntimeException("Invalid XML tree: Attribute is not a compound: " + childTerm.args[0]);
               }
               if (childTerm.args[1] instanceof CompoundTerm)
                  childTerm = (CompoundTerm)childTerm.args[1];
               else if (TermConstants.emptyListAtom.equals(childTerm.args[1]))
                  break;
               else
                  throw new RuntimeException("Invalid XML tree: Attributes is not a list: " + childTerm);
            }
         }
      }
      if (!TermConstants.emptyListAtom.equals(arg3))
      {
         if (!(arg3 instanceof CompoundTerm))
            throw new RuntimeException("Invalid XML tree: children is not a compound: " + arg3);
         CompoundTerm childTerm = (CompoundTerm)arg3;
         if (childTerm.tag == TermConstants.listTag)
         {
            while(childTerm.tag.arity == 2)
            {
               children.add(PrologNode.instantiateNode(childTerm.args[0]));
               if (childTerm.args[1] instanceof CompoundTerm)
                  childTerm = (CompoundTerm)childTerm.args[1];
               else if (TermConstants.emptyListAtom.equals(childTerm.args[1]))
                  break;
               else
                  throw new RuntimeException("Invalid XML tree: Children is not a list: " + childTerm);
            }
         }
      }
   }

   public String getAttribute(String key)
   {
      String value = (String)attributes.get(key);
      if (value == null)
         return "";
      return value;
   }
}
