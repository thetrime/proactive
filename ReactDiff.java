import java.io.*;
import javax.xml.parsers.*;
import org.w3c.dom.*;
import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;

public class ReactDiff
{
   public static PatchSet diff(Document a, Document b)
   {
      PatchSet patchSet = new PatchSet(a.getFirstChild());
      walk(a.getFirstChild(), b.getFirstChild(), patchSet, 0);      
      return patchSet;
   }

   public static boolean nodesAreEqual(Node node1, Node node2)
   {
      if (node1 == null && node2 == null)
         return true;
      else if (node1 == null)
         return false;
      else
         return node1.equals(node2);  
   }

   private static boolean isThunk(Node n)
   {
      return false;
   }
   private static boolean isWidget(Node n)
   {
      return false;
   }

   private static void thunks(Node a, Node b, PatchSet patch, int index)
   {
      return;
   }

   private static void clearState(Node a, PatchSet patch, int index)
   {
      return;
   }
 
   private static List<ReactEdit> appendPatch(List<ReactEdit> apply, ReactEdit patch)
   {
      if (apply != null)
      {
         apply.add(patch);
         return apply;
      }
      else
      {
         List<ReactEdit> p = new LinkedList<ReactEdit>();
         p.add(patch);
         return p;
      }
   }

   private static Map<String, Node> diffProps(NamedNodeMap a, NamedNodeMap b)
   {
      Map<String, Node> diff = null;
      for (int i = 0; i < a.getLength(); i++)
      {
         Attr attr = (Attr)a.item(i);
         String aKey = attr.getName();
         if (b.getNamedItem(aKey) == null)
         {
            if (diff == null)
               diff = new HashMap<String, Node>();
            diff.put(aKey, null);
         }
         Node aValue = a.getNamedItem(aKey);
         Node bValue = b.getNamedItem(aKey);
         if (nodesAreEqual(aValue, bValue))
            continue;
         else // FIXME: There is actually special handling in React for this. We are missing the else-if isObject case. This makes things overly aggressive here
         {
            if (diff == null)
               diff = new HashMap<String, Node>();
            diff.put(aKey, bValue);
         }
      }
      for (int i = 0; i < b.getLength(); i++)
      {
         Attr attr = (Attr)b.item(i);
         String bKey = attr.getName();
         if (a.getNamedItem(bKey) == null)
         {
            if (diff == null)
               diff = new HashMap<String, Node>();
            diff.put(bKey, b.getNamedItem(bKey));
         }
      }
      return diff;
   }
   
   private static void walk(Node a, Node b, PatchSet patch, int index)
   {
      if (nodesAreEqual(a, b))
         return;
      List<ReactEdit> apply = patch.get(index);
      boolean applyClear = false;
      if (isThunk(a) || isThunk(b))
      {
         thunks(a, b, patch, index);
      }
      else if (b == null)
      {
         if (!isWidget(a))
         {
            clearState(a, patch, index);
            apply = patch.get(index);
         }
         apply = appendPatch(apply, new ReactEdit(ReactEdit.REMOVE, a, b));
      }
      else if (b instanceof Element)
      {
         if (a instanceof Element)
         {
            if (a.getNodeName().equals(b.getNodeName()) && ((Element)a).getAttribute("key").equals(((Element)b).getAttribute("key")))
            {
               Map<String,Node> propsPatch = diffProps(((Element)a).getAttributes(), ((Element)b).getAttributes());
               if (propsPatch != null)
               {
                  apply = appendPatch(apply, new ReactEdit(ReactEdit.PROPS, a, propsPatch));
               }
               apply = diffChildren(a, b, patch, apply, index);
            }
            else
            {
               apply = appendPatch(apply, new ReactEdit(ReactEdit.NODE, a, b));
               applyClear = true;
            }
         }
         else
         {
            apply = appendPatch(apply, new ReactEdit(ReactEdit.NODE, a, b));
            applyClear = true;
         }         
      }
      else if (b instanceof Text)
      {
         if (!(a instanceof Text))
         {
            apply = appendPatch(apply, new ReactEdit(ReactEdit.TEXT, a, b));
            applyClear = true;
         }
         else if (!((Text)a).getWholeText().equals(((Text)b).getWholeText()))
         {
            apply = appendPatch(apply, new ReactEdit(ReactEdit.TEXT, a, b));
         }
      }
      else if (isWidget(b))
      {
         if (!isWidget(a))
            applyClear = true;
         apply = appendPatch(apply, new ReactEdit(ReactEdit.WIDGET, a, b));
      }
      if (apply != null)
         patch.put(index, apply);
      if (applyClear)
         clearState(a, patch, index);
   }

   public static List<ReactEdit> diffChildren(Node a, Node b, PatchSet patch, List<ReactEdit> apply, int index)
   {
      NodeList aChildren = a.getChildNodes();
      OrderedSet orderedSet = reorder(aChildren, b.getChildNodes());
      Iterator<Node> bChildren = orderedSet.iterator();
      int aLen = aChildren.getLength();
      int bLen = orderedSet.size();
      int len = aLen > bLen ? aLen : bLen;
      for (int i = 0; i < len; i++)
      {
         Node leftNode = aChildren.item(i);
         Node rightNode = null;
         // We may have exhausted bChildren since len is max(aLen, bLen)
         if (bChildren.hasNext())
            rightNode = bChildren.next();
         index++;
         if (leftNode == null)
         {
            if (rightNode != null)
            {
               // Exccess nodes in b need to be added
               apply = appendPatch(apply, new ReactEdit(ReactEdit.INSERT, null, rightNode));
            }
         }
         else
         {
            walk(leftNode, rightNode, patch, index);
         }
         // FIXME: What is THIS all about?
         if ((leftNode instanceof Element) && ((Element)leftNode).getAttribute("count").length() > 0)
         {
            index += Integer.parseInt(((Element)leftNode).getAttribute("count")); // ??
         }
      }

      Moves moves = orderedSet.moves;
      if (moves != null)
      {
         apply = appendPatch(apply, new ReactEdit(ReactEdit.ORDER, a, moves));
      }
      return apply;
   }

   private static String getKey(Node node)
   {
      return null;
   }
   
   private static KeyIndex keyIndex(NodeList children)
   {
      HashMap<String,Integer> keys = new HashMap<String, Integer>();
      LinkedList<Integer> free = new LinkedList<Integer>();
      for (int i = 0; i < children.getLength(); i++)
      {
         Node child = children.item(i);
         String key = getKey(child);
         if (key != null)
            keys.put(key, i);
         else
            free.push(i);
      }
      return new KeyIndex(keys, free);
   }
   
   private static OrderedSet reorder(NodeList aChildren, NodeList bChildren)
   {
      KeyIndex bChildIndex = keyIndex(bChildren);
      HashMap<String,Integer> bKeys = bChildIndex.keys;
      LinkedList<Integer> bFree = bChildIndex.free;

      if (bFree.size() == bChildren.getLength())
         return new OrderedSet(bChildren, null);

      KeyIndex aChildIndex = keyIndex(aChildren);
      HashMap<String,Integer> aKeys = aChildIndex.keys;
      LinkedList<Integer> aFree = aChildIndex.free;

      if (aFree.size() == aChildren.getLength())
         return new OrderedSet(bChildren, null);

      LinkedList<Node> newChildren = new LinkedList<Node>();
      int freeIndex = 0;
      int freeCount = bFree.size();
      int deletedItems = 0;
      
      for (int i = 0; i < aChildren.getLength(); i++)
      {
         Node aItem = aChildren.item(i);
         Integer itemIndex;
         String aKey = getKey(aItem);
         if (aKey != null)
         {
            if (bKeys.containsKey(aKey))
            {
               itemIndex = bKeys.get(aKey);
               newChildren.push(bChildren.item(itemIndex.intValue()));
            }
            else
            {
               itemIndex = i - deletedItems++;
               newChildren.push(null);
            }
         }
         else
         {
            if (freeIndex < freeCount)
            {
               itemIndex = bFree.get(freeIndex++);
               newChildren.push(bChildren.item(itemIndex.intValue()));
            }
            else
            {
               itemIndex = i - deletedItems++;
               newChildren.push(null);
            }
         }
      }

      int lastFreeIndex = freeIndex >= bFree.size()?bChildren.getLength():bFree.get(freeIndex);
      for (int j = 0; j < bChildren.getLength(); j++)
      {
         Node newItem = bChildren.item(j);
         String newKey = getKey(newItem);
         if (newKey != null)
         {
            if (aKeys.containsKey(newKey))
               newChildren.push(newItem);
         }
         else if (j >= lastFreeIndex)
            newChildren.push(newItem);
      }

      List<Node> simulate = new LinkedList<Node>();
      simulate.addAll(newChildren.subList(0, newChildren.size()));
      int simulateIndex = 0;
      LinkedList<Remove> removes = new LinkedList<Remove>();
      LinkedList<Insert> inserts = new LinkedList<Insert>();
      Node simulateItem;

      for (int k = 0; k < bChildren.getLength(); )
      {
         Node wantedItem = bChildren.item(k);
         simulateItem = simulate.get(simulateIndex);

         while (simulateItem == null && simulate.size() > 0)
         {
            removes.push(remove(simulate, simulateIndex, null));
            simulateItem = simulate.get(simulateIndex);
         }

         if (simulateItem == null || getKey(simulateItem) != getKey(wantedItem))
         {
            String wantedItemKey = getKey(wantedItem);
            if (wantedItemKey != null)
            {
               if (simulateItem != null && getKey(simulateItem) != null)
               {
                  if (bKeys.get(getKey(simulateItem)) != k+1)
                  {
                     removes.push(remove(simulate, simulateIndex, getKey(simulateItem)));
                     simulateItem = simulate.get(simulateIndex);
                     if (simulateItem == null || getKey(simulateItem) != getKey(wantedItem))
                     {
                        inserts.push(new Insert(getKey(wantedItem), k));
                     }
                     else
                     {
                        simulateIndex++;
                     }
                  }
                  else
                  {
                     inserts.push(new Insert(getKey(wantedItem), k));
                  }
               }
               else
               {
                  inserts.push(new Insert(getKey(wantedItem), k));
               }
               k++;
            }
            else if (simulateItem != null && getKey(simulateItem) != null)
            {
               removes.push(remove(simulate, simulateIndex, getKey(simulateItem)));
            }
         }
         else
         {
            simulateIndex++;
            k++;
         }
      }

      while (simulateIndex < simulate.size())
      {
         simulateItem = simulate.get(simulateIndex);
         String simulateItemKey = null;
         if (simulateItem != null)
            simulateItemKey = getKey(simulateItem);
         removes.push(remove(simulate, simulateIndex, simulateItemKey));
      }

      if (removes.size() == deletedItems && inserts.size() == 0)
         return new OrderedSet(newChildren, null);
      return new OrderedSet(newChildren, new Moves(removes, inserts));
      
   }

   public static Remove remove(List<Node> arr, int index, String key)
   {
      arr.removeAll(arr.subList(index, 1));
      return new Remove(index, key);
   }

   
   private static class OrderedSet extends LinkedList<Node>
   {
      public Moves moves;
      public OrderedSet(List<Node> children, Moves moves)
      {
         addAll(children);
         this.moves = moves;
      }
      public OrderedSet(NodeList children, Moves moves)
      {
         for (int i = 0; i < children.getLength(); i++)
            add(children.item(i));
         this.moves = moves;
      }

   }
   private static class Moves
   {
      public List<Remove> removes;
      public List<Insert> inserts;
      public Moves(List<Remove> removes, List<Insert> inserts)
      {
         this.removes = removes;
         this.inserts = inserts;
      }
   }

   private static class Remove
   {
      public int index;
      public String key;
      public Remove(int index, String key)
      {
         this.index = index;
         this.key = key;
      }
   }
   
   private static class Insert
   {
      public int to;
      public String key;
      public Insert(String key, int to)
      {
         this.to = to;
         this.key = key;
      }
   }


   
   private static class KeyIndex
   {
      public HashMap<String,Integer> keys;
      public LinkedList<Integer> free;
      public KeyIndex(HashMap<String,Integer> keys, LinkedList<Integer> free)
      {
         this.keys = keys;
         this.free = free;
      }
   }   

   public static void main(String[] args) throws Exception
   {
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      DocumentBuilder builder = factory.newDocumentBuilder();
      Document baseDocument = builder.parse(new FileInputStream(args[0]));
      Document nextDocument = builder.parse(new FileInputStream(args[1]));
      System.out.println(diff(baseDocument, nextDocument));
      System.exit(-1);
   }
}

/*
 var h = require('virtual-dom/h')
 var x = require('virtual-dom/vtree/diff')(h('Panel', {}, [h('Title', {label:"This is a title specified in XML"}, []), h('Field', {label:"CP Code"}, []), h('Field', {label:"CP Account ID"}, []), h('Button', {label:"Submit"}, [])]), h('Panel', {}, [h('Panel', {}, [h('Title', {label:"This is a title specified in XML"}, []), h('Field', {label:"CP Code"}, []), h('Field', {label:"CP Account ID"}, [])]), h('Button', {label:"Submit"}, [])]));
*/


// js gets 2, 2, 7, 7
// We get 6, 6, 6, 7, 7
