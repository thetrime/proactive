import java.awt.Component;
import java.awt.BorderLayout;
import javax.swing.*;
import java.util.*;


public class ReactApp extends JFrame implements ReactComponent
{
   List<ReactComponent> children = new LinkedList<ReactComponent>();
   public ReactApp(String rootElementId) throws Exception
   {
      super("React Test");
      // This is a bit finicky. First we have to set up the state as 'empty'.
      // The empty state is not as empty as you might think. It contains 2 nodes:
      //    * The global root. This is the representation of this JFrame
      //    * Inside this is a RootPanel. This is like the contentPane in the frame
      // Unlike in Swing, we can change the contentPane to a new one by patching it
      // but the global domRoot is immutable. In reality, we should only EVER have one
      // child here, otherwise Swing goes a bit... well, weird.
      
      getContentPane().setBackground(java.awt.Color.GREEN);
      getContentPane().setLayout(new BorderLayout());
      
      ReactComponent contentPane = new RootPanel(rootElementId);
      insertChildBefore(contentPane, null);
      contentPane.getContext().reRender();
   }

   public PrologContext getContext() { return null; }
   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      if (sibling == null)
      {
         child.setParentNode(this);
         children.add(child);
         getContentPane().add((Component)child, BorderLayout.CENTER);
      }      
   }
   public void removeChild(ReactComponent child)
   {
      children.remove(child);
      getContentPane().remove((Component)child);
   }  
   public ReactComponent getParentNode() { return this; }
   public void replaceChild(ReactComponent newNode, ReactComponent oldNode)
   {
      children.remove(oldNode);
      getContentPane().remove((Component)oldNode);
      insertChildBefore(newNode, null);
   }
   public void setParentNode(ReactComponent parent) {}
   public ReactComponent getOwnerDocument() { return this; }
   public void setOwnerDocument(ReactComponent owner) {}
   public List<ReactComponent> getChildNodes() { return children; }
   public int getFill() { return 0; }
   public void setProperty(String name, Object value) {}
}
