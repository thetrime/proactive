package org.proactive.ui;

import java.awt.Component;
import java.awt.BorderLayout;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import java.util.List;
import java.util.LinkedList;
import java.net.URI;
import org.proactive.prolog.Engine;
import org.proactive.prolog.PrologContext;
import org.proactive.ReactComponent;
import org.proactive.CodeChangeListener;
import org.proactive.React;
import org.proactive.ReactComponentFactory;
import org.proactive.vdom.PrologWidget;

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

              
      // we want to end up calling instantiateNode() with a prologWidget containing <rootElement>
      context = new PrologContext(rootElementId, engine);
      ReactComponent contentPane = ReactComponentFactory.instantiateNode(new PrologWidget(rootElementId), context);
      insertChildBefore(contentPane, null);      
      //      contentPane.getContext().reRender();
      frame.setSize(800, 600);
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      frame.setVisible(true);
   }

   public void handleCodeChange() 
   {
      try
      {
         if (context != null)
         {
            context.getEngine().make();
            context.reRender();
            SwingUtilities.invokeLater(new Runnable()
               {
                  public void run()
                  {
                     frame.validate();
                     frame.repaint();
                     
                  }
               });      
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
