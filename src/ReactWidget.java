package org.proactive;

import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.vm.TermConstants;
import java.util.List;
import java.util.LinkedList;
import java.util.HashMap;
import java.awt.Component;
import java.awt.GridBagConstraints;
import javax.swing.JPanel;
import java.awt.BorderLayout;

public class ReactWidget extends ReactComponent
{
   protected Engine engine;
   protected String elementId;
   protected Term state;
   protected Term props;
   protected Term vDom = null;

   private List<ReactComponent> children = new LinkedList<ReactComponent>();
   private ReactComponent child;

   public ReactWidget(Engine engine, String elementId, Term props) throws Exception
   {
      this.engine = engine;
      this.elementId = elementId;
      this.props = props;

      // First, get the state
      this.state = engine.getInitialState(elementId, props);

      // Then render the initial vDOM
      vDom = engine.render(elementId, state, props);

      // But we cannot just realize the vDOM->DOM directly. Instead, we must compute diffs from a known state and apply those to a known DOM
      // Start by creating an initial contentPane.
      // FIXME: Can we come up with something that does not need ui.Panel? Like ReactComponent(); ?
      child = new org.proactive.ui.Panel("root panel for " + this);

      setProperties(Engine.termToProperties(props));
      // Then we add the contentPane to the widget. This means the parent of the contentPane is the widget itself
      insertChildBefore(child, null);
      // Now we make an equivalent VDOM for the empty widget
      Term emptyVDom = new CompoundTerm("element", new Term[]{AtomTerm.get("Panel"), props, TermConstants.emptyListAtom});
      // Compute the diffs
      Term patches = engine.diff(emptyVDom, vDom);
      // And ask for them to be realized
      React.queuePatch(patches, child, engine);
//      engine.applyPatch(patches, child);
   }

   public Component getAWTComponent()
   {
      return child.getAWTComponent();
   }
   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      this.child = child;
      child.setOwnerDocument(this);
      child.setParentNode(this);
      if (getParentNode() != null)
         getParentNode().replaceChild(this, this);

   }
   public void removeChild(ReactComponent child)
   {
      this.child = null;
      child.setOwnerDocument(null);
      if (getParentNode() != null)
         getParentNode().replaceChild(this, this);

   }
   public void replaceChild(ReactComponent newNode, ReactComponent oldNode)
   {
      if (elementId.equals("foo"))
         System.out.println("Replacing child " + oldNode + " --> " + newNode + " of " + this);
      this.child = newNode;
      child.setOwnerDocument(this);
      child.setParentNode(this);
      if (getParentNode() != null)
         getParentNode().replaceChild(this, this);
   }
   public List<ReactComponent> getChildNodes()
   {
      List<ReactComponent> list = new LinkedList<ReactComponent>();
      if (child != null)
         list.add(child);
      return list;
   }

   public String toString()
   {
      return "<Widget:" + elementId + " " + props+ ">";
   }


   public void setOwnerDocument(ReactWidget owner)
   {
      if (getChildNodes() != null)
         for (ReactComponent child: getChildNodes())
            child.setOwnerDocument(this);
      this.owner = owner;
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

   public void reRender() throws Exception
   {
      Term newvDom = engine.render(elementId, state, props);
      Term patches = engine.diff(vDom, newvDom);
      System.out.println("Patch: " + patches);
      React.queuePatch(patches, child, engine);
      vDom = newvDom;
   }

   public void updateWidget(Term newvDom) throws Exception
   {
      Term patches = engine.diff(vDom, newvDom);
      System.out.println("update widget with: " + patches);
      React.queuePatch(patches, child, engine);
      vDom = newvDom;
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
}
