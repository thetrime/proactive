package org.proactive;

import gnu.prolog.database.*;
import gnu.prolog.io.*;
import gnu.prolog.term.*;
import gnu.prolog.vm.*;
import java.io.*;
import java.util.*;

public class Predicate_module extends ExecuteOnlyCode
{
   public RC execute(Interpreter interpreter, boolean backtrackMode, gnu.prolog.term.Term args[]) throws PrologException
   {
      ReactEnvironment environment = (ReactEnvironment)interpreter.getEnvironment();
      AtomTerm moduleName = (AtomTerm)args[0];
      environment.pushModule(moduleName.value);
      RC rc = Predicate_call.staticExecute(interpreter, false, args[1]);
      // Actually we need a backtrack point here to recover it...
      environment.popModule();
      return rc;
   }
}
