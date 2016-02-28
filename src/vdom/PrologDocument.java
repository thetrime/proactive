package org.proactive.vdom;

import gnu.prolog.term.*;
import java.util.List;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.Engine;

public class PrologDocument extends PrologElement
{
   Term root;
   PrologContext context;
   public PrologDocument(Term term, Term state, Term props, String componentName, Engine engine) throws Exception
   {
      super(term);
      this.context = new PrologContext(state, props, componentName, this, engine);
   }
   public PrologContext getContext()
   {
      return context;
   }      
}
