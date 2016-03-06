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
import org.proactive.ReactComponentFactory;
import org.proactive.vdom.PrologWidget;
import org.proactive.prolog.PrologState;

import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.vm.TermConstants;

public class ReactApp extends ReactComponent implements CodeChangeListener
{
   private Engine engine = null;
   List<ReactComponent> children = new LinkedList<ReactComponent>();
   String URL = null;
   String rootElementId = null;
   private ReactComponent dom;
   Term vDom = null;
   JFrame frame = new JFrame("React Test");
   Term state = TermConstants.emptyListAtom;
   Term props = TermConstants.emptyListAtom;


   public ReactApp(String URL, String rootElementId) throws Exception
   {
      engine = new Engine(URL, rootElementId);
      this.URL = URL;
      this.rootElementId = rootElementId;
      React.addCodeChangeListener(new URI(URL + "/listen"), rootElementId, this);
      frame.getContentPane().setLayout(new BorderLayout());

      // This is a bit tricky because there is no way to take a vDom and make a DOM out of it directly
      // So instead, we make a DOM and a vDom ourselves, manually, that we know correspond
      // Then, we create the vDOM we want, and compute the diffs between the two
      // and apply that to the DOM we mocked up to generate the DOM we actually need.

      // So, first create an initial vDOM of <Panel/>
      vDom = new CompoundTerm("element", new Term[]{AtomTerm.get("Panel"), TermConstants.emptyListAtom, TermConstants.emptyListAtom});

      // And manually create a real DOM which corresponds to that
      dom = ReactComponentFactory.createElement("Panel");

      // Put the document inside the contentPane so we can see it
      insertChildBefore(dom, null);

      // Now we have to initialize the widget manually too. First, get the initial state
      state = engine.getInitialState(rootElementId, props);

      // Now create a patch from that initial state to the output of render() on <RootElementId/>
      Term newvDom = engine.render(rootElementId, state, props);
      Term patches = engine.diff(vDom, newvDom);

      // Update the vDom to be this new structure
      vDom = newvDom;

      // And apply the patches to mutate the real DOM
      React.queuePatch(patches, dom, engine);

      // Then make everything visible while we wait for Swing to apply the patches
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
   public void setProperties(HashMap<String, PrologObject> properties)
   {
   }

   public JFrame getAWTComponent()
   {
      return frame;
   }
}
