package org.proactive.prolog;

import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.term.FloatTerm;
import gnu.prolog.term.RationalTerm;
import gnu.prolog.term.BigIntegerTerm;
import gnu.prolog.term.NumericTerm;
import gnu.prolog.term.IntegerTerm;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.CompoundTermTag;
import gnu.prolog.term.VariableTerm;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.vm.Interpreter;
import gnu.prolog.vm.ExecuteOnlyCode;
import gnu.prolog.vm.PrologException;
import gnu.prolog.vm.Environment;
import gnu.prolog.vm.Evaluate;
import gnu.prolog.io.PrologStream;
import gnu.prolog.io.WriteOptions;
import gnu.prolog.io.OperatorSet;
import gnu.prolog.io.TermWriter;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Arrays;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.io.PrintStream;
import java.io.ByteArrayOutputStream;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;

public class Predicate_format extends ExecuteOnlyCode
{
   public static final CompoundTermTag tagAtom1 = CompoundTermTag.get("atom", 1);
   public static final CompoundTermTag formatErrorTag = CompoundTermTag.get("format_error", 1);
   public static final CompoundTermTag errorTag = CompoundTermTag.get("error", 2);
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Environment environment = interpreter.getEnvironment();
      List<Term> formatArgs = new LinkedList<Term>();
      ByteArrayOutputStream bos = null;
      Term target = null;
      BufferedOutputStream ps = null;
      BufferedOutputStream last_output = null;
      ByteArrayOutputStream tab_buffer = null;
      if (args[0] instanceof CompoundTerm && ((CompoundTerm)args[0]).tag == tagAtom1)
      {
         CompoundTerm sink = (CompoundTerm)args[0];
         target = sink.args[0].dereference();
         bos = new ByteArrayOutputStream();
         ps = new StreamFilter(bos);
      }
      else
      {
         PrologStream stream = interpreter.getEnvironment().resolveStream(args[0]);
         ps = new PrologStreamAdapter(stream, interpreter);
      }

      if (!(args[1] instanceof AtomTerm))
         PrologException.typeError(TermConstants.atomAtom, args[1]);      
      AtomTerm formatString = (AtomTerm)args[1];
      CompoundTerm.toCollection(args[2], formatArgs);
      try
      {
         boolean is_locale_format = false;
         int tab_stop = 0;
         int tab_base = 0;
	 Iterator<Term> a = formatArgs.iterator();
         byte[] input = formatString.value.getBytes("ISO-8859-1");
         int r = -1;
         char tab_character = ' ';
         for(int i = 0; i < input.length; i++)
         {
            if (input[i] == '~')
            {
               if (input[i+1] == '~')
               {
                  ps.print("~");
                  i++;
               }
               else
               {
                  i++;
                  while(true) // This is really a goto, which Java does not support. Only the '0-9' case has a continue
                              // all other cases break at the end of the switch statement
                  {
                     switch(input[i])
                     {
                        case 'a': // atom
                        {
                           Term arg = a.next();
			   if (arg instanceof AtomTerm)
                              ps.print(((AtomTerm)arg).value);
                           else
                              PrologException.typeError(AtomTerm.get("atom"), arg);
                           break;
                        }
                        case 'c': // character code
                        {
                           Term arg = a.next();
                           if (!(arg instanceof IntegerTerm))
                              PrologException.typeError(AtomTerm.get("integer"), arg);
                           if (((IntegerTerm)arg).value >= 0 && ((IntegerTerm)arg).value < 255)
                              ps.print(new String(new char[]{(char)((IntegerTerm)arg).value}));
                           else
                              PrologException.representationError(AtomTerm.get("character_code"));
                           break;
                        }
                        case 'd': // decimal
                        {
                           Term arg = a.next();
                           DecimalFormat df;
                           if (arg instanceof IntegerTerm || arg instanceof BigIntegerTerm)
                           {
                              if (is_locale_format)
                                 df = new DecimalFormat("###,###");
                              else
                                 df = new DecimalFormat("###");
                              if (arg instanceof IntegerTerm)
                                 ps.print(df.format(((IntegerTerm)arg).value));
                              else
                                 ps.print(df.format(((BigIntegerTerm)arg).value));
                              break;
                           }
                           else
                              PrologException.typeError(AtomTerm.get("integer"), arg);
                        }
                        case 'D':
                        {
                           Term arg = a.next();
                           DecimalFormat df = new DecimalFormat("###,###");
                           if (arg instanceof IntegerTerm)
                              ps.print(df.format(((IntegerTerm)arg).value));
                           else if (arg instanceof BigIntegerTerm)
                              ps.print(df.format(((BigIntegerTerm)arg).value));
                           else
                              PrologException.typeError(AtomTerm.get("integer"), arg);
                           break;
                        }
                        case 'e': // floating point as exponential
                        case 'E': // floating point as exponential in upper-case
                        case 'f': // floating point as non-exponential
                        case 'g': // shorter of e or f
                        case 'G': // shorter of E or f
                        {
                           NumericTerm arg = (NumericTerm)Evaluate.evaluate(a.next()); // raises an exception if not evaluable
                           String s;
                           if (r == -1)
                           {
                              if (is_locale_format)
                                 s = "###,##0.000000";
                              else
                                 s = "##0.000000";

                           }
                           else if (r == 0)
                           {
                              if (is_locale_format)
                                 s = "###,##0";
                              else
                                 s = "##0";
                           }
			   else
                           {
                              if (is_locale_format)
                                 s = "###,##0.";
                              else
                                 s = "##0.";
			      for (int k = 0; k < r; k++)
				 s = s + "0";
			   }
			   DecimalFormat df = new DecimalFormat(s);
                           if (arg instanceof FloatTerm)
                              ps.print(df.format(((FloatTerm)arg).value));
			   else if (arg instanceof IntegerTerm)
                              ps.print(df.format(((IntegerTerm)arg).value));
			   else if (arg instanceof BigIntegerTerm)
                              ps.print(df.format(((BigIntegerTerm)arg).value));
			   else if (arg instanceof RationalTerm)
                              ps.print(df.format(((RationalTerm)arg).value.doubleValue()));
                           break;
                        }
                        case 'i': // ignore
                        {
                           a.next();
                           break;
                        }
                        case 'I':
                        {
                           Term arg = a.next();
                           DecimalFormatSymbols symbols = new DecimalFormatSymbols();
                           symbols.setGroupingSeparator('_');
                           String s;
                           if (r == -1)
                              s = "###,###";
                           else if (r == 0)
                              s = "###";
                           else
                           {
                              s = "";
                              for (int k = 0; k < r; k++)
                                 s = s + "#";
                              s = s + "," + s;
                           }
                           DecimalFormat df = new DecimalFormat(s, symbols);
                           if (arg instanceof IntegerTerm)
                              ps.print(df.format(((IntegerTerm)arg).value));
                           else if (arg instanceof BigIntegerTerm)
                              ps.print(df.format(((BigIntegerTerm)arg).value));
                           else
                              PrologException.typeError(AtomTerm.get("integer"), arg);
                           break;
                        }
                        case 'n': // newline
                        {
                           ps.print("\n");
                           break;
                        }
                        case 'N': // soft newline
                        {
                           if (ps.lastChar() != '\n')
                              ps.print("\n");
                           break;
                        }
                        case 'p': // print
                        {
                           WriteOptions options = new WriteOptions(new OperatorSet());
                           options.ignoreOps = false;
                           options.quoted = false;
                           options.numbervars = true;
                           PrintWriter pw = new PrintWriter(ps);
                           new TermWriter(pw).print(options, a.next());
                           pw.flush();
                           break;
                        }
                        case 'q': // writeq
                        {
                           WriteOptions options = new WriteOptions(new OperatorSet());
                           options.ignoreOps = false;
                           options.quoted = true;
                           options.numbervars = true;
                           PrintWriter pw = new PrintWriter(ps);
                           new TermWriter(pw).print(options, a.next());
                           pw.flush();
                           break;
                        }
                        case 'r': // radix
                        case 'R': // uppercase radix
                        {
                           Term arg = a.next();
                           String s = null;
                           if (arg instanceof IntegerTerm)
                           {
                              if (((IntegerTerm)arg).value >= 0)
                                 s = Integer.toHexString(((IntegerTerm)arg).value);
                              else
                              {
                                 ps.print("-");
                                 s = Integer.toHexString(-((IntegerTerm)arg).value);
                              }
                           }
                           else if (arg instanceof BigIntegerTerm)
                           {
                              if (((BigIntegerTerm)arg).value.signum() == -1)
                              {
                                 ps.print("-");
                                 s = (((BigIntegerTerm)arg).value.negate()).toString(16);
                              }
                              else
                                 s = (((BigIntegerTerm)arg).value).toString(16);
                           }
                           else
                              PrologException.typeError(AtomTerm.get("integer"), arg);
                           if (input[i] == 'R')
                              s = s.toUpperCase();
                           if (is_locale_format)
                           {
                              int len = s.length();
                              int k = len % 3;
                              if (k == 0) k = 3;
                              for (int j = 0; j < len;)
                              {
                                 ps.print(s.charAt(j));
                                 if (j+3 < len)
                                    ps.print(",");
                                 j += k;
                                 k = 3;
                              }
                           }
                           else
                              ps.print(s);
                           break;
                        }
                        case 's': // string
                        case '@': // execute
                        {
                           // FIXME: Not implemented
                           PrologException.systemError();
                        }
                        case '|': // reset-tab-stop
                        {
                           tab_stop = ps.length();
                           break;
                        }
                        case '+': // create-tab-stop
                        {
                           int pad_length;
                           if (r == -1)
                              pad_length = 6 - ps.length() - tab_base;
                           else
                              pad_length = r - ps.length() - tab_base;
                           String pad = null;
                           if (pad_length > 0)
                           {
                              char[] chars = new char[pad_length];
                              Arrays.fill(chars, tab_character);
                              pad = new String(chars);
                           }
                           if (last_output == null)
                           {
                              // Not buffering
                              if (pad != null)
                                 ps.print(pad);
                           }
                           else
                           {
                              // left-pad
                              if (pad != null)
                                 last_output.print(pad);
                              ps.flush();
                              ps = last_output;
                              ps.write(tab_buffer.toByteArray());
                              last_output = null;
                           }
                           tab_stop = ps.length();
                           break;
                        }
                        case 't': // tab
                        {
                           last_output = ps;
                           tab_buffer = new ByteArrayOutputStream();
                           ps = new StreamFilter(tab_buffer);
                           tab_base = last_output.length() - tab_stop;
                           if (tab_base < 0)
                              tab_base = 0;
                           tab_character = (char)r;
                           break;
                        }
                        case 'w': // write
                        {
                           WriteOptions options = new WriteOptions(new OperatorSet());
                           options.ignoreOps = false;
                           options.quoted = false;
                           options.numbervars = true;
                           PrintWriter pw = new PrintWriter(ps);
                           new TermWriter(pw).print(options, a.next());
                           pw.flush();
                           break;
                        }
                        case '*':
                        {
                           Term arg = a.next();
                           if (arg instanceof IntegerTerm)
                              r = ((IntegerTerm)arg).value;
                           else
                              PrologException.typeError(TermConstants.integerAtom, arg);
                           i++;
                           continue;
                        }
                        case '`':
                        {
                           r = input[i+1];
                           i+=2;
                           continue;
                        }
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
                        case ':':
                           is_locale_format = true;
                           i++;
                           continue;
                        default:
                           throw new PrologException(new CompoundTerm(formatErrorTag, AtomTerm.get("No such format character: " + input[i])), null);
                     }
                     break;
                  }
               }
            }
            else
               ps.write((char)input[i]);
         }
         ps.flush();
         if (last_output != null)
         {
            last_output.flush();
            ps = last_output;
            ps.write(tab_buffer.toByteArray());
         }
         if (ps.getError() != null)
            throw ps.getError();
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
      catch(IOException ioe)
      {
         PrologException.systemError(ioe);
      }
      return RC.FAIL;
   }

   private abstract class BufferedOutputStream extends OutputStream
   {
      public abstract int lastChar();
      public abstract int length();
      protected PrologException error = null;
      public void print(int b) throws IOException
      {
         write(b);
      }
      public void print(String s) throws IOException
      {
         byte buf[] = s.getBytes();
         write(buf, 0, buf.length);
      }
      public PrologException getError()
      {
         return error;
      }
   }

   private class StreamFilter extends BufferedOutputStream
   {
      private OutputStream sink;
      int lastChar = -1;
      int length = 0;
      public StreamFilter(OutputStream o)
      {
         sink = o;
      }
      public int lastChar()
      {
         return lastChar;
      }
      public int length()
      {
         return length;
      }
      public void write(int b)
      {
         try
         {
            sink.write(b);
            lastChar = b;
            length++;
         }
         catch(IOException ex)
         {
            try
            {
               PrologException.systemError(ex);
            }
            catch(PrologException p)
            {
               error = p;
            }
         }
      }
      public void close()
      {
         try
         {
            sink.close();
         }
         catch(IOException ex)
         {
            try
            {
               PrologException.systemError(ex);
            }
            catch(PrologException p)
            {
               error = p;
            }
         }
      }
      
      public void flush()
      {
         try
         {
            sink.flush();
         }
         catch(IOException ex)
         {
            try
            {
               PrologException.systemError(ex);
            }
            catch(PrologException p)
            {
               error = p;
            }
         }
      }


   }

   // Adapts a prolog stream so it implements the same interface as the ByteArrayOutputStream
   // This lets us write to either an atom or a stream
   private class PrologStreamAdapter extends BufferedOutputStream
   {
      PrologStream sink;
      Interpreter interpreter;
      int lastChar = -1;
      int length = 0;
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
         catch(PrologException notAllowed)
         {
            error = notAllowed;
         }
      }
      
      public void flush()
      {
         try
         {
            sink.flushOutput(null);
         }
         catch(PrologException notAllowed)
         {
            error = notAllowed;
         }
      }

      public void write(int b) throws IOException
      {
         try
         {
            sink.putCode(null, interpreter, b);
            lastChar = b;
            length++;
         }
         catch(PrologException notAllowed)
         {
            error = notAllowed;
         }
      }

      public int lastChar()
      {
         return lastChar;
      }
      public int length()
      {
         return length;
      }

   }
}
