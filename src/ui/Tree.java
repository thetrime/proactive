package org.proactive.ui;

import javax.swing.JTree;
import java.util.List;
import java.awt.Component;
import java.util.HashMap;
import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;

public class Tree extends ReactLeafComponent 
{
   JTree tree = new JTree();
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      if (properties.containsKey("fill"))
         fill = properties.get("fill").asFill();
   }
   public Component getAWTComponent()
   {
      return tree;
   }
}
