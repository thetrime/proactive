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

public class ReactWidget extends ReactComponent
{
   protected Engine engine;
   protected String elementId;
   protected Term state;
   protected Term props;
   protected Term vDom = null;
   // The widget always has 0 or 1 children. The list is just here for efficiency
   List<ReactComponent> children = new LinkedList<ReactComponent>();
   ReactComponent child = null;

   public ReactWidget(Engine engine, String elementId, Term props)
   {
      this.engine = engine;
      this.elementId = elementId;
      this.props = props;
   }

   public void initialize() throws Exception
   {
      // First, get the state
      this.state = engine.getInitialState(elementId, props);

      // Then render the initial vDOM
      vDom = engine.render(elementId, state, props);

      // But we cannot just realize the vDOM->DOM directly. Instead, we must compute diffs from a known state and apply those to a known DOM
      // Start by creating an initial contentPane.
      // FIXME: Can we come up with something that does not need ui.Panel? Like ReactComponent(); ?
      ReactComponent initialDom = ReactComponentFactory.createElement("Panel");
      // Then we add the contentPane to the widget. This means the parent of the contentPane is the widget itself
      insertChildBefore(initialDom, null);
      // Now we make an equivalent VDOM for the empty widget
      Term emptyVDom = new CompoundTerm("element", new Term[]{AtomTerm.get("Panel"), TermConstants.emptyListAtom, TermConstants.emptyListAtom});
      // Compute the diffs
      Term patches = engine.diff(emptyVDom, vDom);
      // And ask for them to be realized
      React.queuePatch(patches, initialDom, engine);
   }

   public Component getAWTComponent()
   {
      return child.getAWTComponent();
   }
   public List<ReactComponent> getChildNodes()
   {
      return children;
   }
   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      children.clear();
      this.child = child;
      children.add(child);
      child.setParentNode(this);
   }
   public void removeChild(ReactComponent child)
   {
      children.clear();
      this.child = null;
   }
   public void replaceChild(ReactComponent newNode, ReactComponent oldNode)
   {
      children.clear();
      this.child = newNode;
      children.add(newNode);
   }

   public String toString()
   {
      return "<Widget:" + elementId + ">";
   }
}
