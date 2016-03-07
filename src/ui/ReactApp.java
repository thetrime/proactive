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
import org.proactive.prolog.PrologObject;
import org.proactive.ReactComponent;
import org.proactive.CodeChangeListener;
import org.proactive.React;
import org.proactive.WidgetContext;
import org.proactive.ReactComponentFactory;
import org.proactive.prolog.PrologState;

import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.vm.TermConstants;

public class ReactApp extends JFrame implements CodeChangeListener
{
   String URL = null;
   private Engine engine;
   private WidgetContext context;
   public ReactApp(String URL, String rootElementId) throws Exception
   {
      super("React Test");
      engine = new Engine(URL, rootElementId);
      context = new WidgetContext(null, engine, rootElementId, TermConstants.emptyListAtom);
      React.addCodeChangeListener(new URI(URL + "/listen"), rootElementId, this);
      getContentPane().setLayout(new BorderLayout());
      ReactComponent component = context.init();
      getContentPane().add(context.init().getAWTComponent(), BorderLayout.CENTER);
      setSize(800, 600);
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      setVisible(true);
   }

   public void handleCodeChange() 
   {
      SwingUtilities.invokeLater(new Runnable()
         {
            public void run()
            {
               try
               {
                  engine.make();
                  context.reRender();
                  validate();
                  repaint();
               }
               catch(Exception e)
               {
                  e.printStackTrace();
               }
            }
         });
   }
}
