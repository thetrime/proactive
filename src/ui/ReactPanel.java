package org.proactive.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.BorderLayout;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import java.util.List;
import java.util.HashMap;
import java.util.LinkedList;
import java.net.URI;
import org.proactive.prolog.Engine;
import org.proactive.prolog.PrologObject;
import org.proactive.ReactComponent;
import org.proactive.StyleSheetListener;
import org.proactive.React;
import org.proactive.StyleSheet;
import org.proactive.ReactWidget;
import org.proactive.ReactComponentFactory;
import org.proactive.prolog.PrologState;
import org.proactive.HTTPContext;
import org.proactive.Version;
import org.proactive.DisconnectionListener;

import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.vm.Environment;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.vm.PrologException;

public class ReactPanel extends JPanel implements StyleSheetListener
{
   String URL = null;
   private Engine engine;
   private ReactWidget context;
   public ReactPanel(String URL, String rootElementId, HTTPContext httpContext, DisconnectionListener listener) throws Exception
   {
      System.out.println("Proactive v" + Version.version + " is ready");
      StyleSheet sheet = new StyleSheet();
      sheet.setValueForClass("title", "colour", java.awt.Color.WHITE);
      sheet.setValueForClass("title", "font-size", 24);
      React.setStyleSheet(sheet);
      engine = new Engine(URL, rootElementId, httpContext, listener);
      context = new ReactWidget(null, engine, rootElementId, PrologState.emptyState);
      engine.setRootWidget(context);
      React.addStyleSheetListener(this);
      setLayout(new BorderLayout());
      add(context.getAWTComponent(), BorderLayout.CENTER);
      addComponentListener(new ComponentAdapter()
         {
            public void componentResized(ComponentEvent e)
            {
               try
               {
                  context.reRender();
                  validate();
                  repaint();
               }
                catch(Exception f)
               {
                  f.printStackTrace();
               }
            }
         });
   }

   public void setProps(PrologState props) throws PrologException
   {
      context.updateWidget(props);
   }

   public Environment getEnvironment()
   {
      return engine.getEnvironment();
   }

   public void styleSheetChanged()
   {
      SwingUtilities.invokeLater(new Runnable()
         {
            public void run()
            {
               try
               {
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
   public void cleanUp()
   {
      // FIXME: Currently a no-op
   }

}
