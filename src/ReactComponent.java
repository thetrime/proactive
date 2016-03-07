package org.proactive;

import org.proactive.prolog.PrologObject;
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
         if (properties.get("fill") == null)
            fill = GridBagConstraints.NONE;
         else
            fill = properties.get("fill").asFill();
      }
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
      child.setOwnerDocument(owner);
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
      child.setOwnerDocument(null);
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
      newChild.setOwnerDocument(owner);
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
   }
   public ReactWidget getOwnerDocument()
   {
      return owner;
   }
   public void setOwnerDocument(ReactWidget owner)
   {
      if (getChildNodes() != null)
         for (ReactComponent child: getChildNodes())
            child.setOwnerDocument(owner);
      this.owner = owner;
   }
   public int getFill()
   {
      return fill;
   }
}
