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
      this.state = engine.getInitialState(componentName);
      this.props = null;
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
      this.root = root;
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
