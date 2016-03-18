package org.proactive;

import org.proactive.prolog.PrologObject;
import org.proactive.ReactWidget;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;
import java.awt.Component;
import java.awt.GridBagConstraints;

public abstract class ReactComponent
{
   protected ReactComponent parent;
   protected ReactWidget owner;
   protected HashMap<String, PrologObject> properties = new HashMap<String, PrologObject>();
   protected List<ReactComponent> children = new LinkedList<ReactComponent>();
   protected HashMap<ReactComponent, Component> awtMap = new HashMap<ReactComponent, Component>();

   protected String id = null;
   protected String className = null;

   protected int fill = GridBagConstraints.NONE;
   public ReactComponent()
   {
   }

   public abstract Component getAWTComponent();
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      for (Map.Entry<String, PrologObject> property : properties.entrySet())
         properties.put(property.getKey(), property.getValue());
      if (properties.containsKey("fill"))
      {
         int oldFill = fill;
         if (properties.get("fill") == null)
            fill = GridBagConstraints.NONE;
         else
            fill = properties.get("fill").asFill();
         if (oldFill != fill && getParentNode() != null)
            getParentNode().replaceChild(this, this);
      }
      if (properties.containsKey("className"))
         className = properties.get("className").asString();
      if (properties.containsKey("id"))
         id = properties.get("id").asString();

   }
   
   public List<ReactComponent> getChildNodes()
   {
      return children;
   }
   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      // First rehome the child in the document
      if (child.getParentNode() != null)
         child.getParentNode().removeChild(child);
      child.setParentNode(this);
      awtMap.put(child, child.getAWTComponent());

      // Now put it in the right place in the list
      int index = (sibling==null)?-1:children.indexOf(sibling);
      if (index == -1)
         children.add(child);
      else
         children.add(index, child);
   }
   public void removeChild(ReactComponent child)
   {
      awtMap.remove(child);
      child.setParentNode(null);
      children.remove(child);
   }
   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      int i = children.indexOf(oldChild);
      if (i == -1)
      {
         System.out.println("Attempted to replace " + oldChild + " in " + this + " but it is not currently a child!");
         System.out.println("Children are: ");
         System.out.println(children);
      }
      children.set(i, newChild);
      newChild.setParentNode(this);
      awtMap.remove(oldChild);
      awtMap.put(newChild, newChild.getAWTComponent());

   }
   
   public ReactComponent getParentNode()
   {
      return parent;
   }
   
   public void setParentNode(ReactComponent parent)
   {
      this.parent = parent;
      if (parent == null) // destroy subtree
         for (ReactComponent child: children)
            child.setParentNode(null);
   }
   public ReactWidget getOwnerDocument()
   {
      return owner;
      /*
      ReactWidget c = this.owner;
      ReactComponent n = getParentNode();
      while (c == null)
      {
         if (n == null)
         {
            System.out.println("Component " + this + " has no eventual owner...");
            return null;
         }
         c = n.getOwnerDocument();
         n = n.getParentNode();
      }
      return c;
      */
   }
   public void setOwnerDocument(ReactWidget owner)
   {
      this.owner = owner;
      for (ReactComponent child: children)
         child.setOwnerDocument(owner);
   }
   public int getFill()
   {
      return fill;
   }
}
