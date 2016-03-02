package org.proactive.ui;

import javax.swing.JFrame;
import java.util.List;
import java.util.LinkedList;
import java.awt.Component;
import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactComponent;

public class Tab extends ReactComponent
{
   private ReactComponent child = null;
   private String label = "???";
   private String tooltip = "???";
   List<ReactComponent> children = new LinkedList<ReactComponent>();

   public Tab(PrologNode n, PrologContext context)
   {
      super(context);
   }
   public void setProperty(String name, PrologObject value)
   {
      if (name.equals("label"))
         label = value.asString();
      else if (name.equals("tooltip"))
         tooltip = value.asString();
   }
   public Component getAWTComponent()
   {
      return child.getAWTComponent();
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      // First rehome the child in the document
      this.child = child;
      if (child.getParentNode() != null)
         child.getParentNode().removeChild(child);
      child.setParentNode(this);
      child.setOwnerDocument(owner);
      children.clear();
      children.add(child);
   }

   public void removeChild(ReactComponent child)
   {
      if (child == this.child)
      {
         this.child = null;
         children.clear();
      }
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      if (this.child == oldChild)
      {
         this.child = newChild;
         children.clear();
         children.add(newChild);
      }
   }

   public List<ReactComponent> getChildNodes()
   {
      return children;
   }

   public String getLabel()
   {
      return label;
   }

   public String getTooltip()
   {
      return tooltip;
   }
}
