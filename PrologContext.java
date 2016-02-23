import gnu.prolog.term.*;

public class PrologContext
{
   PrologState state;
   PrologState props;
   String componentName;
   PrologDocument document;
   ReactComponent root;

   public PrologContext(String componentName)
   {
      this.state = null;
      this.props = null;
      this.componentName = componentName;
      this.document = null;
   }
   
   public PrologContext(Term state, Term props, String componentName, PrologDocument document)
   {
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
      state = React.engine.triggerEvent(handler, state, props);
      reRender();
   }

   public void reRender() throws Exception
   {
      PrologDocument newDocument = React.engine.render(componentName, state, props);
      PatchSet editScript = ReactDiff.diff(document, newDocument);
      React.queuePatch(editScript, root, this);
   }
}
