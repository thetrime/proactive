package org.proactive.prolog;

import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.io.*;
import java.util.*;

public class Predicate_java_println extends ExecuteOnlyCode
{
   private static int id = 0;
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      Environment environment = interpreter.getEnvironment();
      PrologStream stream;
      System.out.println(args[0]);
      return RC.SUCCESS_LAST;
   }
}
