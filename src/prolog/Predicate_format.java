package org.proactive.prolog;

import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.FloatTerm;
import gnu.prolog.term.NumericTerm;
import gnu.prolog.term.IntegerTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.CompoundTermTag;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.Environment;
import gnu.prolog.io.PrologStream;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.io.OutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.io.PrintStream;
import java.io.ByteArrayOutputStream;
import java.text.DecimalFormat;

public class Predicate_format extends ExecuteOnlyCode
{
   public static final CompoundTermTag tagAtom1 = CompoundTermTag.get("atom", 1);
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Environment environment = interpreter.getEnvironment();
      List<Term> formatArgs = new LinkedList<Term>();
      PrintStream ps = null;
      ByteArrayOutputStream bos = null;
      Term target = null;
      if (args[0] instanceof CompoundTerm && ((CompoundTerm)args[0]).tag == tagAtom1)
      {
         CompoundTerm sink = (CompoundTerm)args[0];
         target = sink.args[0];
         bos = new ByteArrayOutputStream();
         ps = new PrintStream(bos);
      }
      else
      {
         PrologStream stream = interpreter.getEnvironment().resolveStream(args[0]);
         ps = new PrintStream(new PrologStreamAdapter(stream, interpreter));
      }

      if (!(args[1] instanceof AtomTerm))
         PrologException.typeError(TermConstants.atomAtom, args[1]);      
      AtomTerm formatString = (AtomTerm)args[1];
      CompoundTerm.toCollection(args[2], formatArgs);
      try
      {
         Iterator a = formatArgs.iterator();
         byte[] input = formatString.value.getBytes("ISO-8859-1");
         int r = 0;
         for(int i = 0; i < input.length; i++)
         {
            if (input[i] == '~')
            {
               if (input[i+1] == '~')
                  ps.print('~');
               else
               {
                  i++;
                  while(true) // This is really a goto, which Java does not support. Only the '0-9' case has a continue
                              // all other cases break at the end of the switch statement
                  {
                     switch(input[i])
                     {
                        case 'D':
                        {
                           int v = ((IntegerTerm)a.next()).value;
                           DecimalFormat df = new DecimalFormat("###,###");
                           ps.print(df.format(v));
                           break;
                        }
                        case 'w':
                        case 'p':
                        case 'q':
                        case 'f':
                        {
                           Term arg = (Term)a.next();
                           if (arg instanceof AtomTerm)
                              ps.print(((AtomTerm)arg).value);
                           else if (arg instanceof NumericTerm)
                           {
                              String s;
                              if (r == 0)
                                 s = "###,##0";
                              else
                              {
                                 s = "###,##0.";
                                 for (int k = 0; k < r; k++)
                                    s = s + "0";
                              }
                              DecimalFormat df = new DecimalFormat(s);
                              if (arg instanceof FloatTerm)
                                 ps.print(df.format(((FloatTerm)arg).value));
                              else if (arg instanceof IntegerTerm)
                                 ps.print(df.format(((IntegerTerm)arg).value));
                           }
                           else
                              ps.print(arg.toString());
                        }
                        break;
                        case 'n':
                           ps.println();
                           break;
                        case '0':
                        case '1':
                        case '2':
                        case '3':
                        case '4':
                        case '5':
                        case '6':
                        case '7':
                        case '8':
                        case '9':
                        {
                           r = input[i++] - '0';                        
                           while (input[i] >= '0' && input[i] <= '9')
                           {
                              r = 10 * r + input[i++] - '0';
                           }
                           continue; // ie goto top
                        }
                        case 'X':
                        {
                           Term arg = (Term)a.next();
                           double val = 0;
                           if (arg instanceof IntegerTerm)
                              val = ((IntegerTerm)arg).value;
                           else if (arg instanceof FloatTerm)
                              val = ((FloatTerm)arg).value;
                           String s;
                           if (r == 0)
                              s = "###,##0";
                           else
                           {
                              s = "###,##0.";
                              for (int k = 0; k < r; k++)
                                 s = s + "0";
                           }                           
                           DecimalFormat df = new DecimalFormat(s);
                           ps.print(df.format(val));
                           break;
                        }
                        default:
                           ps.print("<??" + input[i] + "?>");
                     }
                     break;
                  }
               }
            }
            else
               ps.write((char)input[i]);
         }
         ps.flush();
         if (bos != null)
         {
            AtomTerm data = AtomTerm.get(bos.toString("ISO-8859-1"));
            return interpreter.unify(target, data);
         }
         else
         {
            return RC.SUCCESS_LAST;
         }
      }
      catch(UnsupportedEncodingException uee)
      {
         return RC.FAIL;
      }
   }

   // Adapts a prolog stream so it implements the same interface as the ByteArrayOutputStream
   // This lets us write to either an atom or a stream
   private class PrologStreamAdapter extends OutputStream
   {
      PrologStream sink;
      Interpreter interpreter;
      public PrologStreamAdapter(PrologStream sink, Interpreter interpreter)
      {
         this.sink = sink;
         this.interpreter = interpreter;
      }

      public void close()
      {
         try
         {
            sink.close(true);
         } 
         catch(Exception e) {}
      }
      
      public void flush() 
      {
         try
         {
            sink.flushOutput(null);
         } 
         catch(Exception e) {}
      }

      public void write(int b) 
      {
         try
         {
            sink.putCode(null, interpreter, b);
         } 
         catch(Exception e) {}
      }
   }
}
