package org.proactive;

import java.util.List;
import java.util.LinkedList;

public abstract class ReactLeafComponent extends ReactComponent
{
   public List<ReactComponent> children = new LinkedList<ReactComponent>();
   public void insertChildBefore(ReactComponent child, ReactComponent sibling) {}
   public void removeChild(ReactComponent child) {}
   public void replaceChild(ReactComponent newChild, ReactComponent oldChild) {}
   public List<ReactComponent> getChildNodes() { return children; }
}
