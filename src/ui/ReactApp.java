package org.proactive.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.BorderLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import java.util.List;
import java.util.HashMap;
import java.util.LinkedList;
import java.net.URI;
import org.proactive.prolog.Engine;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import org.proactive.ReactComponent;
import org.proactive.CodeChangeListener;
import org.proactive.React;
import org.proactive.ReactWidget;
import org.proactive.ReactComponentFactory;
import org.proactive.vdom.PrologWidget;
import org.proactive.prolog.PrologState;

import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.vm.TermConstants;

public class ReactApp extends ReactWidget implements CodeChangeListener
{
   String URL = null;
   JFrame frame = new JFrame("React Test");
   List<ReactComponent> children = new LinkedList<ReactComponent>();
   ReactComponent contentPane;

   public ReactApp(String URL, String rootElementId) throws Exception
   {
      super(new Engine(URL, rootElementId), rootElementId, TermConstants.emptyListAtom);
      React.addCodeChangeListener(new URI(URL + "/listen"), rootElementId, this);
      contentPane = ReactComponentFactory.createElement("Panel");
      children.add(contentPane);
      initialize();
      frame.getContentPane().setLayout(new BorderLayout());
      contentPane.setParentNode(this);
      frame.getContentPane().add(contentPane.getAWTComponent(), BorderLayout.CENTER);
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
      contentPane.insertChildBefore(child, sibling);
   }
   public void removeChild(ReactComponent child)
   {
      contentPane.removeChild(child);
   }
   public void replaceChild(ReactComponent newNode, ReactComponent oldNode)
   {
      contentPane.replaceChild(newNode, oldNode);
   }
   public List<ReactComponent> getChildNodes()
   {
      return children;
   }

   public JFrame getAWTComponent()
   {
      return frame;
   }
}
