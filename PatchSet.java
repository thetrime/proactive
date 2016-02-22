import java.util.*;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Document;

public class PatchSet extends HashMap<Integer, List<ReactEdit>>
{
   Node a;
   public PatchSet(Node a)
   {
      this.a = a;
   }

   public ReactComponent apply(ReactComponent rootNode)
   {
      return applyRecursive(rootNode);
   }

   private ReactComponent applyRecursive(ReactComponent rootNode)
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

   private ReactComponent applyPatch(ReactComponent rootNode, ReactComponent domNode, List<ReactEdit> patchList)
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

   private HashMap<Integer, ReactComponent> domIndex(ReactComponent rootNode, Node tree, Set<Integer> indices)
   {
      HashMap<Integer, ReactComponent> result = new HashMap<Integer, ReactComponent>();
      if (indices.size() == 0)
         return result;
      return recurse(rootNode, tree, indices.toArray(new Integer[0]), result, 0);
   }

   private HashMap<Integer, ReactComponent> recurse(ReactComponent rootNode, Node tree, Integer[] indices, HashMap<Integer, ReactComponent> nodes, int rootIndex)
   {
      if (rootNode != null)
      {
         if (indexInRange(indices, rootIndex, rootIndex))
            nodes.put(rootIndex, rootNode);
      }
      if (tree != null && tree.getChildNodes().getLength() > 0)
      {
         NodeList vChildren = tree.getChildNodes();
         List<ReactComponent> childNodes = rootNode.getChildNodes();
         for (int i = 0; i < tree.getChildNodes().getLength(); i++)
         {
            rootIndex++;
            Node vChild = (i > vChildren.getLength())?null:vChildren.item(i);
            String count = (vChild == null)?"":((Element)vChild).getAttribute("count"); 
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
