package org.proactive;

import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.awt.Component;
import java.awt.GridBagConstraints;

public abstract class ReactComponent
{
   protected PrologContext context;
   protected ReactComponent parent;
   protected ReactWidget owner;
   protected HashMap<String, PrologObject> properties = new HashMap<String, PrologObject>();
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
   
   public abstract List<ReactComponent> getChildNodes();
   public abstract void insertChildBefore(ReactComponent child, ReactComponent sibling);   
   public abstract void removeChild(ReactComponent child);         
   public abstract void replaceChild(ReactComponent newNode, ReactComponent oldNode);
   
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
   public PrologContext getContext()
   {
      return context;
   }
}
