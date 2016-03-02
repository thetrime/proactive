package org.proactive;

import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import java.util.List;
import java.awt.Component;
import java.awt.GridBagConstraints;

public abstract class ReactComponent
{
   protected PrologContext context;
   protected ReactComponent parent;
   protected ReactComponent owner;

   protected int fill = GridBagConstraints.NONE;
   public ReactComponent(PrologContext context)
   {
      this.context = context;
   }

   public abstract Component getAWTComponent();
   public abstract void setProperty(String name, PrologObject value);
   
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
   public ReactComponent getOwnerDocument()
   {
      return owner;
   }
   public void setOwnerDocument(ReactComponent owner)
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
