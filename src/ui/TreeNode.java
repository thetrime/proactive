package org.proactive.ui;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import java.util.List;
import java.util.Enumeration;
import java.awt.Component;
import java.util.HashMap;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactComponent;

public class TreeNode extends ReactComponent
{
   private DefaultTreeModel model;
   private DefaultMutableTreeNode node = null;

   public TreeNode()
   {
      node = new DefaultMutableTreeNode("");
   }

   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
      if (properties.containsKey("label"))
      {
         node.setUserObject(properties.get("label").asString());
         if (model != null)
            model.nodeChanged(node);
      }

   }

   public Component getAWTComponent()
   {
      return null;
   }

   public DefaultMutableTreeNode getNode()
   {
      return node;
   }

   public void setModel(DefaultTreeModel m)
   {
      this.model = m;
      for (ReactComponent c : children)
         ((TreeNode)c).setModel(m);
   }


   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      super.insertChildBefore(child, sibling);
      if (child instanceof TreeNode)
      {
         ((TreeNode)child).setModel(model);
         int index = 0;
         if (sibling != null)
            index = node.getIndex(((TreeNode)sibling).getNode());
         node.insert(((TreeNode)child).getNode(), index);
         if (model != null)
            model.nodeStructureChanged(node);
      }
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      int index = node.getIndex(((TreeNode)oldChild).getNode());
      super.replaceChild(newChild, oldChild);
      node.remove(((TreeNode)oldChild).getNode());
      node.insert(((TreeNode)newChild).getNode(), index);
      if (model != null)
         model.nodeStructureChanged(node);
   }

   public void removeChild(ReactComponent child)
   {
      super.removeChild(child);
      if (child instanceof TreeNode)
      {
         node.remove(((TreeNode)child).getNode());
         if (model != null)
            model.nodeStructureChanged(node);
      }
   }

}
