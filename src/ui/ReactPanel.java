package org.proactive.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.BorderLayout;
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
import org.proactive.StyleSheetListener;
import org.proactive.React;
import org.proactive.StyleSheet;
import org.proactive.ReactWidget;
import org.proactive.ReactComponentFactory;
import org.proactive.prolog.PrologState;
import org.proactive.HTTPContext;

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
   public ReactPanel(String URL, String rootElementId, HTTPContext httpContext) throws Exception
   {
      StyleSheet sheet = new StyleSheet();
      sheet.setValueForClass("title", "colour", java.awt.Color.WHITE);
      sheet.setValueForClass("title", "font-size", 24);
      React.setStyleSheet(sheet);
      engine = new Engine(URL, rootElementId, httpContext);
      context = new ReactWidget(null, engine, rootElementId, PrologState.emptyState);
      engine.setRootWidget(context);
      React.addStyleSheetListener(this);
      setLayout(new BorderLayout());
      add(context.getAWTComponent(), BorderLayout.CENTER);
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
   public void cleanUp()
   {
      // FIXME: Currently a no-op
   }

}
