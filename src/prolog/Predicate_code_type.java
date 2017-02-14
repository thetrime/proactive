package org.proactive.prolog;

import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.vm.BacktrackInfo;
import gnu.prolog.term.Term;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.AtomicTerm;
import gnu.prolog.term.IntegerTerm;
import gnu.prolog.term.CompoundTermTag;
import java.util.Vector;

public class Predicate_code_type extends ExecuteOnlyCode
{
   public static final AtomTerm alnumAtom = AtomTerm.get("alnum");
   public static final AtomTerm alphaAtom = AtomTerm.get("alpha");
   public static final AtomTerm asciiAtom = AtomTerm.get("ascii");
   public static final AtomTerm cntrlAtom = AtomTerm.get("cntrl");
   public static final AtomTerm csymAtom = AtomTerm.get("csym");
   public static final AtomTerm csymfAtom = AtomTerm.get("csymf");
   public static final AtomTerm digitAtom = AtomTerm.get("digit");
   public static final AtomTerm endOfLineAtom = AtomTerm.get("end_of_line");
   public static final AtomTerm graphAtom = AtomTerm.get("graph");
   public static final AtomTerm lowerAtom = AtomTerm.get("lower");
   public static final AtomTerm newlineAtom = AtomTerm.get("newline");
   public static final AtomTerm periodAtom = AtomTerm.get("period");
   public static final AtomTerm prologAtomStartAtom = AtomTerm.get("prolog_atom_start");
   public static final AtomTerm prologIdentifierContinueAtom = AtomTerm.get("prolog_identifier_continue");
   public static final AtomTerm prologSymbolAtom = AtomTerm.get("prolog_symbol");
   public static final AtomTerm prologVarStartAtom = AtomTerm.get("prolog_var_start");
   public static final AtomTerm punctAtom = AtomTerm.get("punct");
   public static final AtomTerm quoteAtom = AtomTerm.get("quote");
   public static final AtomTerm spaceAtom = AtomTerm.get("space");
   public static final AtomTerm upperAtom = AtomTerm.get("upper");
   public static final AtomTerm whiteAtom = AtomTerm.get("white");
   public static final AtomTerm characterTypeAtom = AtomTerm.get("character_type");

   private static final CompoundTermTag upperFunctor = CompoundTermTag.get("upper", 1);
   private static final CompoundTermTag lowerFunctor = CompoundTermTag.get("lower", 1);
   private static final CompoundTermTag toLowerFunctor = CompoundTermTag.get("toLower", 1);
   private static final CompoundTermTag toUpperFunctor = CompoundTermTag.get("toUpper", 1);
   private static final CompoundTermTag xdigitFunctor = CompoundTermTag.get("xdigit", 1);
   private static final CompoundTermTag digitFunctor = CompoundTermTag.get("digit", 1);
   private static final CompoundTermTag parenFunctor = CompoundTermTag.get("paren", 1);

   private static boolean isxdigit(char c)
   {
      return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
   }

   private static boolean isPunct(char val)
   {
      // Note that this assumes your locale is C. Character.getType() gives a lot of possible flags but I am not sure how to
      // map these to 'punct'
      return val >= 33 && val <= 47
         || val >= 58 && val <= 64
         || val >= 91 && val <= 96
         || val >= 123 && val <= 126;
   }


   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Term code = args[0];
      Term type = args[1];
      if (code instanceof VariableTerm && type instanceof VariableTerm)
         PrologException.instantiationError();
      if (code instanceof VariableTerm)
      {
         // Nondet difficult case. Not currently implemented
         PrologException.instantiationError();
      }
      else
      {
         char cval = 0;
         if (!(code instanceof AtomicTerm))
            PrologException.typeError(TermConstants.atomicAtom, code);
         if (code instanceof AtomTerm)
         {
            if (((AtomTerm)code).value.length() != 1)
               PrologException.typeError(TermConstants.characterCodeAtom, code);
            cval = ((AtomTerm)code).value.charAt(0);
         }
         else if (code instanceof IntegerTerm)
         {
            cval = (char)(((IntegerTerm)code).value);
         }
         else
            PrologException.typeError(TermConstants.characterCodeAtom, code);

         if (!(type instanceof VariableTerm))
         {
            // Deterministic case
            if (type instanceof CompoundTerm)
            {
               CompoundTerm ct = (CompoundTerm)type;
               if (ct.tag == upperFunctor)
               {
                  if (Character.isUpperCase(cval))
                     return interpreter.unify(ct.args[0], IntegerTerm.get(Character.toLowerCase(cval)));
                  return RC.FAIL;
               }
               else if (ct.tag == lowerFunctor)
               {
                  if (Character.isLowerCase(cval))
                     return interpreter.unify(ct.args[0], IntegerTerm.get(Character.toUpperCase(cval)));
                  return RC.FAIL;
               }
               else if (ct.tag == toLowerFunctor)
               {
                  return interpreter.unify(ct.args[0], IntegerTerm.get(Character.toUpperCase(cval)));
               }
               else if (ct.tag == toUpperFunctor)
               {
                  return interpreter.unify(ct.args[0], IntegerTerm.get(Character.toLowerCase(cval)));
               }
               else if (ct.tag == xdigitFunctor)
               {
                  if (isxdigit(cval))
                  {
                     if (cval >= 'a')
                        return interpreter.unify(ct.args[0], IntegerTerm.get(cval - 'a' + 10));
                     else if (cval >= 'A')
                        return interpreter.unify(ct.args[0], IntegerTerm.get(cval - 'A' + 10));
                     else
                        return interpreter.unify(ct.args[0], IntegerTerm.get(cval - '0'));
                  }
                  return RC.FAIL;
               }
               else if (ct.tag == digitFunctor)
               {
                  if (Character.isDigit(cval))
                     return interpreter.unify(ct.args[0], IntegerTerm.get(cval - '0'));
                  return RC.FAIL;
               }
               else if (ct.tag == parenFunctor)
               {
                  if (cval == '(')
                     return interpreter.unify(ct.args[0], IntegerTerm.get(')'));
                  else if (cval == '{')
                     return interpreter.unify(ct.args[0], IntegerTerm.get('}'));
                  else if (cval == '[')
                     return interpreter.unify(ct.args[0], IntegerTerm.get(']'));
                  return RC.FAIL;
               }
               else
                  PrologException.typeError(characterTypeAtom, code);
            }
            else
            {
               if (type == alnumAtom)
                  return (Character.isLetterOrDigit(cval))?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == alphaAtom)
                  return (Character.isAlphabetic(cval))?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == asciiAtom)
                  return (cval <= 127)?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == asciiAtom)
                  return (cval <= 127)?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == cntrlAtom)
                  return (Character.isISOControl(cval))?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == csymAtom)
                  return (Character.isLetterOrDigit(cval) || cval == '_')?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == csymfAtom)
                  return (Character.isAlphabetic(cval) || cval == '_')?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == digitAtom)
                  return (Character.isDigit(cval))?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == endOfLineAtom)
                  return (cval >= 10 && cval <= 13)?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == graphAtom)
                  return (!Character.isISOControl(cval) && cval != ' ')?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == lowerAtom)
                  return (Character.isLowerCase(cval))?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == newlineAtom)
                  return (cval == 10)?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == periodAtom)
                  return (cval == '.' || cval == '?' || cval == '!')?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == prologAtomStartAtom)
                  return (Character.isLowerCase(cval))?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == prologIdentifierContinueAtom)
                  return (Character.isLetterOrDigit(cval) || cval == '_')?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == prologSymbolAtom)
                  return (!Character.isISOControl(cval) && cval != ' ')?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == prologVarStartAtom)
                  return (Character.isUpperCase(cval) || cval == '_')?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == punctAtom)
                  return (isPunct(cval))?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == quoteAtom)
                  return (cval == '"' || cval == '\'' || cval == '`')?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == spaceAtom)
                  return (Character.isSpaceChar(cval))?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == upperAtom)
                  return (Character.isUpperCase(cval))?RC.SUCCESS_LAST:RC.FAIL;
               else if (type == whiteAtom)
                  return (cval == 9 || cval == 32)?RC.SUCCESS_LAST:RC.FAIL;
               else
                  PrologException.typeError(characterTypeAtom, code);
            }
         }
         else
         {
            // Nondeterministic case
            CodeTypeBacktrackInfo bi;
            if (backtrackMode)
               bi = (CodeTypeBacktrackInfo)interpreter.popBacktrackInfo();
            else
            {
               bi = new CodeTypeBacktrackInfo(interpreter);
               if (Character.isLetterOrDigit(cval))
                  bi.add(alnumAtom);
               if (Character.isAlphabetic(cval))
                  bi.add(alphaAtom);
               if (cval <= 127)
                  bi.add(asciiAtom);
               if (Character.isISOControl(cval))
                  bi.add(cntrlAtom);
               if (Character.isLetterOrDigit(cval) || cval == '_')
                  bi.add(csymAtom);
               if (Character.isAlphabetic(cval) || cval == '_')
                  bi.add(csymfAtom);
               if (Character.isDigit(cval))
               {
                  bi.add(digitAtom);
                  bi.add(new CompoundTerm(digitFunctor, new Term[]{IntegerTerm.get(cval - '0')}));
                  bi.add(new CompoundTerm(xdigitFunctor, new Term[]{IntegerTerm.get(cval - '0')}));
               }
               if (isxdigit(cval))
               {
                  if (cval >= 'a')
                     bi.add(new CompoundTerm(xdigitFunctor, new Term[]{IntegerTerm.get(cval - 'a' + 10)}));
                  else if (cval >= 'A')
                     bi.add(new CompoundTerm(xdigitFunctor, new Term[]{IntegerTerm.get(cval - 'A' + 10)}));
               }
               if (cval >= 10 && cval <= 13)
                  bi.add(endOfLineAtom);
               if (!Character.isISOControl(cval) && cval != ' ')
                  bi.add(graphAtom);
               if (Character.isLowerCase(cval))
               {
                  bi.add(lowerAtom);
                  bi.add(new CompoundTerm(lowerFunctor, new Term[]{IntegerTerm.get(Character.toUpperCase(cval))}));
               }
               if (Character.isUpperCase(cval))
               {
                  bi.add(upperAtom);
                  bi.add(new CompoundTerm(upperFunctor, new Term[]{IntegerTerm.get(Character.toLowerCase(cval))}));
               }
               if (cval == 10)
                  bi.add(newlineAtom);
               if (cval == '.' || cval == '!' || cval == '?')
                  bi.add(periodAtom);
               if (Character.isLowerCase(cval))
                  bi.add(prologAtomStartAtom);
               if (Character.isLetterOrDigit(cval) || cval == '_')
                  bi.add(prologIdentifierContinueAtom);
               if (!Character.isISOControl(cval) && cval != ' ')
                  bi.add(prologSymbolAtom);
               if (Character.isUpperCase(cval) || cval == '_')
                  bi.add(prologVarStartAtom);
               if (isPunct(cval))
                  bi.add(punctAtom);
               if (cval == '"' || cval == '\'' || cval == '`')
                  bi.add(quoteAtom);
               if (Character.isSpaceChar(cval))
                  bi.add(spaceAtom);
               if (cval == 9 || cval == 32)
                  bi.add(whiteAtom);
               if (cval == '(')
                  bi.add(new CompoundTerm(parenFunctor, new Term[]{IntegerTerm.get(')')}));
               if (cval == '{')
                  bi.add(new CompoundTerm(parenFunctor, new Term[]{IntegerTerm.get('}')}));
               if (cval == '[')
                  bi.add(new CompoundTerm(parenFunctor, new Term[]{IntegerTerm.get(']')}));
               bi.add(new CompoundTerm(toLowerFunctor, new Term[]{IntegerTerm.get(Character.toUpperCase(cval))}));
               bi.add(new CompoundTerm(toUpperFunctor, new Term[]{IntegerTerm.get(Character.toLowerCase(cval))}));

            }
            while (bi.size() != 0)
            {
               interpreter.undo(bi.startUndoPosition);
               if (interpreter.unify(bi.pop(), args[1]) == RC.FAIL)
                  continue;
               if (bi.size() == 0)
                  return RC.SUCCESS_LAST;
               interpreter.pushBacktrackInfo(bi);
               return RC.SUCCESS;
            }
            return RC.FAIL;
         }
      }
      return RC.FAIL; // Should be unreachable
   }


   public class CodeTypeBacktrackInfo extends BacktrackInfo
   {
      private Vector<Term> list;
      int startUndoPosition;
      public CodeTypeBacktrackInfo(Interpreter interpreter)
      {
         super(-1, -1);
         startUndoPosition = interpreter.getUndoPosition();
         list = new Vector<Term>();
      }
      public int size()
      {
         return list.size();
      }
      public void add(Term t)
      {
         list.add(t);
      }
      public Term pop()
      {
         Term t = list.firstElement();
         list.removeElementAt(0);
         return t;
      }
   }
}
