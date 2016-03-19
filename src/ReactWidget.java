package org.proactive;

import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.prolog.FluxDispatcher;
import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.vm.TermConstants;
import gnu.prolog.vm.PrologException;
import java.util.List;
import java.util.Stack;
import java.util.LinkedList;
import java.util.HashMap;
import java.awt.Component;
import java.awt.GridBagConstraints;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import java.awt.BorderLayout;

/* Important notes
 *    o A ReactWidget does NOT contain any children!
 *    o it DOES however, have a visual representation. Imagine it as a photo of another component
 *    o It has a single reference internalComponent which has its own life. We just grab the AWT component from it when rendering
 */


public class ReactWidget extends ReactComponent implements CodeChangeListener
{
   protected Engine engine;
   protected String elementId;
   protected Term state;
   protected Term props;
   protected Term vDom = null;

   private ReactComponent internalComponent;

   public ReactWidget(ReactWidget parentContext, Engine engine, String elementId, Term props) throws Exception
   {
      this.engine = engine;
      this.elementId = elementId;
      this.props = props;
      this.owner = parentContext;

      setProperties(Engine.termToProperties(props));

      React.addCodeChangeListener(engine.getListenURI(), elementId, this);

      // First, get the state
      this.state = engine.getInitialState(elementId, props);

      // Then render the initial vDOM
      vDom = engine.render(this, elementId, state, props);

      // Now turn that into an actual component
      internalComponent = engine.createElementFromVDom(vDom, this);
      internalComponent.setOwnerDocument(this);

      // Next, check to see if the module needs any flux listeners
      engine.checkForFluxListeners(this);
   }

   public void handleCodeChange() 
   {
      System.out.println("Widget code changed");
      SwingUtilities.invokeLater(new Runnable()
         {
            public void run()
            {
               try
               {
                  engine.make();
                  reRender();
               }
               catch(Exception e)
               {
                  e.printStackTrace();
               }
            }
         });
   }

   public Component getAWTComponent()
   {
      return internalComponent.getAWTComponent();
   }

   public void setParentNode(ReactComponent parent)
   {
      super.setParentNode(parent);
      destroy();
      // Destroy child if we are being destroyed
      if (parent == null && internalComponent != null)
         internalComponent.setParentNode(null);
   }

   public String toString()
   {
      return "<Widget:" + elementId + " " + props+ ">";
   }

   public Term getState()
   {
      return state;
   }

   public Term getProps()
   {
      return props;
   }

   public String getComponentName()
   {
      return elementId;
   }

   public void setState(Term newState) throws PrologException
   {
      state = newState;
      System.out.println("State of " + elementId + " is now " + newState);
      reRender();
   }

   public void reRender() throws PrologException
   {
      Term newvDom = engine.render(this, elementId, state, props);
      Term patches = engine.diff(vDom, newvDom);
      //System.out.println("new VDOM: " + newvDom);
      internalComponent = engine.applyPatch(patches, internalComponent);
      internalComponent.setOwnerDocument(this);
      getAWTComponent().validate();
      vDom = newvDom;
   }

   public void destroy()
   {
      FluxDispatcher.deregisterFluxListener(elementId, this);
   }

   public ReactWidget updateWidget(Term newProps) throws Exception
   {
      //System.out.println("UpdateWidget called on " + elementId + " with props: " + newProps);
      props = newProps;
      reRender();
      return this;
   }


   public boolean triggerEvent(Term handler, Term event) throws PrologException
   {
      return engine.triggerEvent(handler, event, this);
   }

   public ReactComponent renderContextualElement(Term handler, Term event) throws PrologException
   {
      Term elementDom = engine.renderContextualElement(handler, event, this);
      ReactComponent element = engine.createElementFromVDom(elementDom, this);
      element.setOwnerDocument(this);
      return element;
   }


   public boolean fluxEvent(Term goal, String storeName, Term storeState) throws Exception
   {
      return engine.fluxEvent(goal, storeName, storeState, this);
   }

   public Engine getEngine()
   {
      return engine;
   }

   public ReactWidget getParentContext()
   {
      return owner;
   }
}
