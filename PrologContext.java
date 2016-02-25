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
      state = engine.triggerEvent(handler, state, props);
      reRender();
   }

   public void reRender() throws Exception
   {
      PrologDocument newDocument = engine.render(componentName, state, props);
      PatchSet editScript = ReactDiff.diff(document, newDocument);
      React.queuePatch(editScript, root, this);
      document = newDocument;
   }

   public Engine getEngine()
   {
      return engine;
   }
}
