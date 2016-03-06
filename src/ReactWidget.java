package org.proactive;

import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import gnu.prolog.term.Term;
import gnu.prolog.term.CompoundTerm;
import gnu.prolog.term.AtomTerm;
import gnu.prolog.vm.TermConstants;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import java.awt.GridBagConstraints;

public class ReactWidget extends ReactComponent
{
   private Engine engine;
   private String elementId;
   private Term state;
   private Term props;
   private Term vDom = null;
   private ReactComponent dom = null;
   public ReactWidget(Engine engine, String elementId, Term props) throws Exception
   {
      this.engine = engine;
      this.elementId = elementId;
      this.props = props;
      this.state = engine.getInitialState(elementId, props);
      vDom = engine.render(elementId, state, props);
      dom = ReactComponentFactory.createElement("Panel");
      Term patches = engine.diff(new CompoundTerm("element", new Term[]{AtomTerm.get("Panel"), TermConstants.emptyListAtom, TermConstants.emptyListAtom}), vDom);
      React.queuePatch(patches, this, engine);
   }

   public Component getAWTComponent()
   {
      return dom.getAWTComponent();
   }
   public List<ReactComponent> getChildNodes()
   {
      return dom.getChildNodes();
   }
   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      dom.insertChildBefore(child, sibling);
   }
   public void removeChild(ReactComponent child)
   {
      dom.removeChild(child);
   }
   public void replaceChild(ReactComponent newNode, ReactComponent oldNode)
   {
      dom.replaceChild(newNode, oldNode);
   }
}
