package org.proactive;

import org.proactive.prolog.Engine;
import org.proactive.prolog.PrologObject;
import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.CompoundTermTag;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.vm.TermConstants;


public class WidgetContext
{
   protected Engine engine;
   protected String elementId;
   protected Term state;
   protected Term props;
   protected Term vDom = null;
   protected ReactComponent dom = null;
   public WidgetContext(WidgetContext parentContext, Engine engine, String componentId, Term props)
   {
      this.parentContext = parentContext;
      this.engine = engine;
      this.elementId = componentId;
      this.props = props;
      this.state = TermConstants.emptyListAtom;
   }

   // This is a bit complicated. We initially set things up to have this structure:
   //  DOM : Panel -> Panel
   //  VDOM:          Panel
   // We pass in the child element as the DOM. This lets the patch change the 'root' element while still
   // allowing us to get at it.
   // We can then set the DOM to be the new child

   public ReactComponent init() throws Exception
   {
      System.out.println("init() for " + elementId);
      // First, get the state
      this.state = engine.getInitialState(elementId, props);
      // Then render the initial vDOM
      vDom = engine.render(elementId, state, props);

      // But we cannot just realize the vDOM->DOM directly. Instead, we must compute diffs from a known state and apply those to a known DOM
      // Start by creating an initial contentPane.
      // FIXME: Can we come up with something that does not need ui.Panel? Like ReactComponent(); ?
      dom = new org.proactive.ui.Panel("Panel for " + elementId);
      ReactComponent domChild = new org.proactive.ui.Button();
      dom.insertChildBefore(domChild, null);
      dom.setOwnerDocument(this);
      domChild.setOwnerDocument(this);

      // Now we make an equivalent VDOM for the empty widget
      Term emptyVDom = new CompoundTerm("element", new Term[]{AtomTerm.get("Button"), TermConstants.emptyListAtom, TermConstants.emptyListAtom});
      // Compute the diffs
      Term patches = engine.diff(emptyVDom, vDom);
      System.out.println("About to apply patches to " + domChild);
      System.out.println("Dom is " + dom + " and has these children:" + dom.getChildNodes());
      dom = engine.applyPatch(patches, domChild);
      System.out.println("Widget should have this as the DOM: " + dom);
      dom.setOwnerDocument(this);
      return dom;
   }

   public ReactComponent updateWidget(Term oldvDom, Term newvDom, ReactComponent domNode) throws Exception
   {
      setState(???)
      return dom;
   }


   public void triggerEvent(Term handler, PrologObject context) throws Exception
   {
      engine.triggerEvent(handler, context, this);
   }

   public void fluxEvent(Term key, Term value) throws Exception
   {
      Term proposedState = engine.fluxEvent(elementId, key, value, state, props);
      if (proposedState != null)
      {      
         state = proposedState;
         reRender();
      }
   }

   public void reRender() throws Exception
   {
      Term newvDom = engine.render(elementId, state, props);
      Term patches = engine.diff(vDom, newvDom);
      vDom = newvDom;
      System.out.println("rerendering. DOM of " + elementId + " is currently: " + dom);
      dom = engine.applyPatch(patches, dom);
      dom.setOwnerDocument(this);
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

   public void setState(Term newState) throws Exception
   {
      state = newState;
      System.out.println("State is now " + newState);
      reRender();
   }

   WidgetContext parentContext = null;
   public WidgetContext getParentContext()
   {
      return parentContext;
   }

   public String toString()
   {
      return "<Context: " + elementId +">";
   }

   public Engine getEngine()
   {
      return owner;
   }
}

