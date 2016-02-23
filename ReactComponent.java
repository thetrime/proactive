import java.util.List;

public interface ReactComponent
{
   public void insertChildBefore(ReactComponent child, ReactComponent sibling);
   public void removeChild(ReactComponent child);
   public ReactComponent getParentNode();
   public void replaceChild(ReactComponent newNode, ReactComponent oldNode);
   public void setParentNode(ReactComponent parent);
   public ReactComponent getOwnerDocument();
   public void setOwnerDocument(ReactComponent owner);
   public List<ReactComponent> getChildNodes();
   public int getFill();
   public void setProperty(String name, Object value);
   public PrologContext getContext();
}
