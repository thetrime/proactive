package org.proactive.prolog;
import org.proactive.vdom.PrologDocument;
import org.proactive.vdom.PatchSet;
import org.proactive.vdom.ReactDiff;
import org.proactive.ReactComponent;
import org.proactive.React;
import gnu.prolog.term.*;

public class PrologContext
{
   PrologState state;
   PrologState props;
   String componentName;
   PrologDocument document;
   ReactComponent root;
   Engine engine;
   
   public PrologContext(String componentName, Engine engine)
   {
      this.engine = engine;
      this.props = PrologState.emptyState();
      this.state = engine.getInitialState(componentName, props);
      this.componentName = componentName;
      this.document = null;
   }
   
   public PrologContext(Term state, Term props, String componentName, PrologDocument document, Engine engine)
   {
      this.engine = engine;
      this.state = new PrologState(state);
      this.props = new PrologState(props);
      this.componentName = componentName;
      this.document = document;
   }

   public void setRoot(ReactComponent root)
   {
      if (root == null)
         throw new NullPointerException();
      this.root = root;
   }

   public void fluxEvent(Term key, Term value) throws Exception
   {
      PrologState proposedState = engine.fluxEvent(componentName, key, value, state, props);
      if (proposedState != null)
      {      
         // We get null if the handler failed
         state = proposedState;
         reRender();
      }
   }

   public void triggerEvent(Object handler) throws Exception
   {
      PrologState proposedState = engine.triggerEvent(componentName, handler, state, props);
      if (proposedState != null)
      {
         // We get null if the handler failed
         state = proposedState;
         reRender();
      }
   }

   public void reRender() throws Exception
   {
      System.out.println("+++++++ Rerendering " + componentName);
      PrologDocument newDocument = engine.render(componentName, state, props);
      PatchSet editScript = ReactDiff.diff(document, newDocument);
      React.queuePatch(editScript, root, this);
      document = newDocument;
   }

   // this is just a convenience method
   public int getFill(Object fillSpec)
   {
      if (fillSpec instanceof String)
      {
         String fill = (String)fillSpec;
         if (fill.equals("horizontal"))
            return java.awt.GridBagConstraints.HORIZONTAL;
         else if (fill.equals("vertical"))
            return java.awt.GridBagConstraints.VERTICAL;
         else if (fill.equals("both"))
            return java.awt.GridBagConstraints.BOTH;
      }
      return java.awt.GridBagConstraints.NONE;      
   }

   
   public Engine getEngine()
   {
      return engine;
   }
}
