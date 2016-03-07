package org.proactive.prolog;
import org.proactive.vdom.PrologDocument;
import org.proactive.vdom.PatchSet;
import org.proactive.vdom.ReactDiff;
import org.proactive.ReactComponent;
import org.proactive.React;
import gnu.prolog.term.Term;
import gnu.prolog.term.AtomTerm;

public class PrologContext
{
   PrologState state;
   PrologState props;
   String componentName;
   PrologDocument document;
   ReactComponent root;
   Engine engine;
   PrologContext parent;
   
   public PrologContext(String componentName, Engine engine)
   {
      this.engine = engine;
      this.props = PrologState.emptyState();
      this.state = new PrologState(engine.getInitialState(componentName, props.getValue()));
      this.componentName = componentName;
      this.document = null;
   }
   
   public PrologContext(Term state, Term props, String componentName, PrologDocument document, Engine engine, PrologContext parent)
   {
      this.engine = engine;
      this.state = new PrologState(state);
      this.props = new PrologState(props);
      this.componentName = componentName;
      this.document = document;
      this.parent = parent;
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

   public void triggerEvent(Object handler, PrologObject context) throws Exception
   {
      engine.triggerEvent(handler, context, null);
   }

   public void setState(PrologState proposedState) throws Exception
   {
      state = proposedState;
      reRender();
   }

   public void reRender() throws Exception
   {
      System.out.println("+++++++ Rerendering " + componentName);
      PrologDocument newDocument = engine.render(componentName, state, props, this);
      PatchSet editScript = ReactDiff.diff(document, newDocument);
      System.out.println("+++++++ WRONG " + componentName);
      //React.queuePatch(editScript, root, this);
      document = newDocument;
   }
   
   public Engine getEngine()
   {
      return engine;
   }

   public PrologContext getParentContext()
   {
      return parent;
   }
}
