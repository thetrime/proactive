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

public class ReactApp extends JFrame implements CodeChangeListener
{
   String URL = null;
   ReactWidget widget;
   private Engine engine;
   public ReactApp(String URL, String rootElementId) throws Exception
   {
      super("React Test");
      engine = new Engine(URL, rootElementId);
      widget = new ReactWidget(engine, rootElementId, TermConstants.emptyListAtom);
      React.addCodeChangeListener(new URI(URL + "/listen"), rootElementId, this);
      getContentPane().setLayout(new BorderLayout());
      getContentPane().add(widget.getAWTComponent(), BorderLayout.CENTER);
      setSize(800, 600);
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      setVisible(true);
   }

   public void handleCodeChange() 
   {
      try
      {
         engine.make();
         //context.reRender();
         SwingUtilities.invokeLater(new Runnable()
            {
               public void run()
               {
                  validate();
                  repaint();

               }
            });
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
   }
}
