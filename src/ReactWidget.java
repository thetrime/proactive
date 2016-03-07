package org.proactive;

import org.proactive.prolog.PrologContext;
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
   private JPanel panel = new JPanel();
   // The widget always has 0 or 1 children. The list is just here for efficiency
   List<ReactComponent> children = new LinkedList<ReactComponent>();
   ReactComponent child = null;

   public ReactWidget(Engine engine, String elementId, Term props) throws Exception
   {
      this.engine = engine;
      this.elementId = elementId;
      this.props = props;
      panel.setLayout(new BorderLayout());

      // First, get the state
      this.state = engine.getInitialState(elementId, props);

      // Then render the initial vDOM
      vDom = engine.render(elementId, state, props);

      // But we cannot just realize the vDOM->DOM directly. Instead, we must compute diffs from a known state and apply those to a known DOM
      // Start by creating an initial contentPane.
      // FIXME: Can we come up with something that does not need ui.Panel? Like ReactComponent(); ?
      ReactComponent child = new org.proactive.ui.Panel("root panel for " + this);

      setProperties(Engine.termToProperties(props, this));
      // Then we add the contentPane to the widget. This means the parent of the contentPane is the widget itself
      insertChildBefore(child, null);
      // Now we make an equivalent VDOM for the empty widget
      Term emptyVDom = new CompoundTerm("element", new Term[]{AtomTerm.get("Panel"), props, TermConstants.emptyListAtom});
      // Compute the diffs
      Term patches = engine.diff(emptyVDom, vDom);
      // And ask for them to be realized
      React.queuePatch(patches, child, engine);
   }

   public Component getAWTComponent()
   {
      return panel;
   }
   public List<ReactComponent> getChildNodes()
   {
      return children;
   }
   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      panel.removeAll();
      panel.add(child.getAWTComponent(), BorderLayout.CENTER);
      children.clear();
      this.child = child;
      children.add(child);
      child.setParentNode(this);
      child.setOwnerDocument(this);
      if (getParentNode() != null)
         getParentNode().replaceChild(this, this);

   }
   public void removeChild(ReactComponent child)
   {
      panel.remove(child.getAWTComponent());
      children.clear();
      this.child = null;
      if (getParentNode() != null)
         getParentNode().replaceChild(this, this);

   }
   public void replaceChild(ReactComponent newNode, ReactComponent oldNode)
   {
      panel.remove(oldNode.getAWTComponent());
      panel.add(newNode.getAWTComponent(), BorderLayout.CENTER);
      children.clear();
      this.child = newNode;
      children.add(newNode);
      newNode.setParentNode(this);
      newNode.setOwnerDocument(this);
      if (getParentNode() != null)
         getParentNode().replaceChild(this, this);

   }

   public String toString()
   {
      return "<Widget:" + elementId + " " + props+ ">";
   }

   public void triggerEvent(Term handler, PrologObject context) throws Exception
   {
      engine.triggerEvent(handler, context, this);
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
      reRender();
   }

   public void reRender() throws Exception
   {
      Term newvDom = engine.render(elementId, state, props);
      Term patches = engine.diff(vDom, newvDom);
      React.queuePatch(patches, child, engine);
      vDom = newvDom;
   }
}
