import java.awt.Component;
import java.awt.BorderLayout;
import javax.swing.*;
import java.util.*;
import java.net.URI;


public class ReactApp extends ReactComponent implements CodeChangeListener
{
   private Engine engine = null;
   List<ReactComponent> children = new LinkedList<ReactComponent>();
   String URL = null;
   String rootElementId = null;
   JFrame frame = new JFrame("React Test");
   public ReactApp(String URL, String rootElementId) throws Exception
   {
      super(null);
      engine = new Engine(URL, rootElementId);
      this.URL = URL;
      this.rootElementId = rootElementId;
      React.addCodeChangeListener(new URI(URL + "/listen"), rootElementId, this);
      
      // This is a bit finicky. First we have to set up the state as 'empty'.
      // The empty state is not as empty as you might think. It contains 2 nodes:
      //    * The global root. This is the representation of this JFrame
      //    * Inside this is a RootPanel. This is like the contentPane in the frame
      // Unlike in Swing, we can change the contentPane to a new one by patching it
      // but the global domRoot is immutable. In reality, we should only EVER have one
      // child here, otherwise Swing goes a bit... well, weird.
      
      frame.getContentPane().setBackground(java.awt.Color.GREEN);
      frame.getContentPane().setLayout(new BorderLayout());
      
      ReactComponent contentPane = new RootPanel(rootElementId, engine);
      insertChildBefore(contentPane, null);
      contentPane.getContext().reRender();
   }

   public void handleCodeChange() 
   {
      try
      {
         if (context != null)
         {
            context.getEngine().make();
            context.reRender();
         }
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {      
      if (sibling == null)
      {
         child.setParentNode(this);
         children.add(child);
         frame.getContentPane().add(child.getAWTComponent(), BorderLayout.CENTER);
         context = child.getContext();
      }
      frame.validate();
      frame.repaint();
   }
   public void removeChild(ReactComponent child)
   {
      context = null;
      children.remove(child);
      frame.getContentPane().remove(child.getAWTComponent());
   }  
   public void replaceChild(ReactComponent newNode, ReactComponent oldNode)
   {
      removeChild(oldNode);
      insertChildBefore(newNode, null);
   }
   public List<ReactComponent> getChildNodes()
   {
      return children;
   }
   public void setProperty(String name, Object value)
   {
   }

   public JFrame getAWTComponent()
   {
      return frame;
   }
}
