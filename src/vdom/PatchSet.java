package org.proactive.vdom;

import org.proactive.ReactComponent;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.TreeSet;
import java.util.Set;
import java.util.Iterator;

public class PatchSet extends HashMap<Integer, List<ReactEdit>>
{
   PrologNode a;
   public PatchSet(PrologNode a)
   {
      this.a = a;
   }

   public ReactComponent apply(ReactComponent rootNode) throws Exception
   {
      return applyRecursive(rootNode);
   }

   private ReactComponent applyRecursive(ReactComponent rootNode) throws Exception
   {
      Set<Integer> indices = new TreeSet<Integer>(keySet());
      if (indices.size() == 0)
         return rootNode;               
      Map<Integer, ReactComponent> index = domIndex(rootNode, a, indices);
      ReactComponent ownerDocument = rootNode.getOwnerDocument();

      for (Iterator<Integer> i = indices.iterator(); i.hasNext();)
      {
         int nodeIndex = i.next().intValue();
         rootNode = applyPatch(rootNode, index.get(nodeIndex), this.get(nodeIndex));
      }
      return rootNode;
   }

   private ReactComponent applyPatch(ReactComponent rootNode, ReactComponent domNode, List<ReactEdit> patchList) throws Exception
   {
      if (domNode == null)
         return rootNode;

      ReactComponent newNode;
      for (Iterator<ReactEdit> i = patchList.iterator(); i.hasNext();)
      {
         newNode = i.next().apply(domNode);
         if (domNode == rootNode)
            rootNode = newNode;
      }
      return rootNode;
   }

   private HashMap<Integer, ReactComponent> domIndex(ReactComponent rootNode, PrologNode tree, Set<Integer> indices)
   {
      HashMap<Integer, ReactComponent> result = new HashMap<Integer, ReactComponent>();
      if (indices.size() == 0)
         return result;
      return recurse(rootNode, tree, indices.toArray(new Integer[0]), result, 0);
   }

   private HashMap<Integer, ReactComponent> recurse(ReactComponent rootNode, PrologNode tree, Integer[] indices, HashMap<Integer, ReactComponent> nodes, int rootIndex)
   {
      if (rootNode != null)
      {
         if (indexInRange(indices, rootIndex, rootIndex))
            nodes.put(rootIndex, rootNode);
      }
      if (tree != null && tree.getChildren().size() > 0)
      {
         List<PrologNode> vChildren = tree.getChildren();
         List<ReactComponent> childNodes = rootNode.getChildNodes();
         for (int i = 0; i < tree.getChildren().size(); i++)
         {
            rootIndex++;
            PrologNode vChild = (i > vChildren.size())?null:vChildren.get(i);
            String count = (vChild == null)?"":((PrologElement)vChild).getAttribute("count"); 
            int nextIndex = rootIndex + ((count.length() == 0)?0:Integer.parseInt(count));
            if (indexInRange(indices, rootIndex, nextIndex))
               recurse(childNodes.get(i), vChild, indices, nodes, rootIndex);
            rootIndex = nextIndex;
         }
      }
      return nodes;
   }

   private boolean indexInRange(Integer[] indices, int left, int right)
   {
      if (indices.length == 0)
         return false;
      int minIndex = 0;
      int maxIndex = indices.length - 1;
      int currentIndex, currentItem;
      while (minIndex <= maxIndex)
      {
         currentIndex = (int)((maxIndex + minIndex) / 2);
         currentItem = indices[currentIndex];
         if (minIndex == maxIndex)
            return currentItem >= left && currentItem <= right;
         else if (currentItem < left)
            minIndex = currentIndex + 1;
         else if (currentIndex > right)
            maxIndex = currentIndex - 1;
         else
            return true;
      }
      return false;
   }
}
