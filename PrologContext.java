import gnu.prolog.term.*;

public class PrologContext
{
    PrologState state;
    PrologState props;
    String componentName;
    PrologDocument document;
    
    public PrologContext(Term state, Term props, String componentName, PrologDocument document)
    {
        this.state = new PrologState(state);
        this.props = new PrologState(props);
        this.componentName = componentName;
        this.document = document;
    }

    public void triggerEvent(Object handler) throws Exception
    {
        state = React.engine.triggerEvent(handler, state, props);
        PrologDocument newDocument = React.engine.render(componentName, state, props);
        PatchSet editScript = ReactDiff.diff(document, newDocument);
        // Hmm. We can only apply a patch to the root, at present...
        // Really updateState(PrologDocument) should be a member function of ReactComponent
    }
}
