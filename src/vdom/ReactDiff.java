package org.proactive.vdom;

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;

public class ReactDiff
{
   public static PatchSet diff(PrologNode a, PrologNode b)
   {
      PatchSet patchSet = new PatchSet(a);
      walk(a, b, patchSet, 0);      
      return patchSet;
   }

   public static boolean nodesAreEqual(PrologNode node1, PrologNode node2)
   {
      if (node1 == null && node2 == null)
         return true;
      else if (node1 == null)
         return false;
      else
         return node1.equals(node2);  
   }

   public static boolean objectsAreEqual(Object node1, Object node2)
   {
      if (node1 == null && node2 == null)
         return true;
      else if (node1 == null)
         return false;
      else
         return node1.equals(node2);  
   }

   private static boolean isThunk(PrologNode n)
   {
      return (n instanceof PrologThunk);
   }
   private static boolean isWidget(PrologNode n)
   {
      // FIXME: Stub
      return false;
   }

   private static void thunks(PrologNode a, PrologNode b, PatchSet patch, int index)
   {
      ThunkPatch p = handleThunk(a, b);
      PatchSet thunkPatch = diff(p.a, p.b);
      if (patch.size() > 0)
      {
         // This nukes any existing patches for this index
         List<ReactEdit> list = new LinkedList<ReactEdit>();
         list.add(new ReactEditThunk(null, thunkPatch));
         patch.put(index, list);
      }
   }

   private static ThunkPatch handleThunk(PrologNode a, PrologNode b)
   {
      PrologNode renderedA = a;
      PrologNode renderedB = b;
      if (isThunk(b))
         renderedB = renderThunk((PrologThunk)b, a);
      if (isThunk(a))
         renderedA = renderThunk((PrologThunk)a, null);
      return new ThunkPatch(renderedA, renderedB);
   }

   private static PrologNode renderThunk(PrologThunk thunk, PrologNode previous)
   {
      PrologNode renderedThunk = thunk.vNode;
      if (renderedThunk == null)
      {
         thunk.vNode = thunk.render(previous);
         renderedThunk = thunk.vNode;
      }
      return renderedThunk;
   }
   
   private static class ThunkPatch
   {
      public PrologNode a;
      public PrologNode b;
      public ThunkPatch(PrologNode a, PrologNode b)
      {
         this.a = a;
         this.b = b;
      }
   }

   private static void clearState(PrologNode a, PatchSet patch, int index)
   {
      // FIXME: Stub
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

   private static Map<String, Object> diffProps(Map<String, Object> a, Map<String, Object> b)
   {
      Map<String, Object> diff = null;
      for (Iterator<Map.Entry<String, Object>> i = a.entrySet().iterator(); i.hasNext(); )
      {
         Map.Entry<String,Object> entry = i.next();
         String aKey = entry.getKey();
         if (!b.containsKey(aKey))
         {
            if (diff == null)
               diff = new HashMap<String, Object>();
            diff.put(aKey, null);
         }
         Object aValue = a.get(aKey);
         Object bValue = b.get(aKey);
         if (objectsAreEqual(aValue, bValue))
            continue;
//         else if (isObject(aValue) && isObject(bValue))
//         {
//            FIXME: There is actually special handling in React for this. We are missing the else-if isObject case. This makes things overly aggressive here
//            System.out.println(">>>>>>>>>>>>> HERE");
//         }
         else 
         {
            if (diff == null)
               diff = new HashMap<String, Object>();
            diff.put(aKey, bValue);
         }
      }
      for (Iterator<Map.Entry<String, Object>> i = b.entrySet().iterator(); i.hasNext();)
      {
         Map.Entry<String,Object> entry = i.next();
         String bKey = entry.getKey();
         if (!a.containsKey(bKey))
         {
            if (diff == null)
               diff = new HashMap<String, Object>();
            diff.put(bKey, entry.getValue());
         }
      }
      return diff;
   }
   
   private static void walk(PrologNode a, PrologNode b, PatchSet patch, int index)
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
         apply = appendPatch(apply, new ReactEditRemove(a, b));
      }
      else if (b instanceof PrologElement)
      {
         if (a instanceof PrologElement)
         {
            if (a.getNodeName().equals(b.getNodeName()) && ((PrologElement)a).getAttribute("key").equals(((PrologElement)b).getAttribute("key")))
            {
               Map<String,Object> propsPatch = diffProps(((PrologElement)a).getAttributes(), ((PrologElement)b).getAttributes());
               if (propsPatch != null)
               {
                  apply = appendPatch(apply, new ReactEditProps(a, propsPatch));
               }
               apply = diffChildren(a, b, patch, apply, index);
            }
            else
            {
               apply = appendPatch(apply, new ReactEditNode(a, b));
               applyClear = true;
            }
         }
         else
         {
            apply = appendPatch(apply, new ReactEditNode(a, b));
            applyClear = true;
         }         
      }
      else if (b instanceof PrologText)
      {
         if (!(a instanceof PrologText))
         {
            apply = appendPatch(apply, new ReactEditText(a, b));
            applyClear = true;
         }
         else if (!((PrologText)a).getWholeText().equals(((PrologText)b).getWholeText()))
         {
            apply = appendPatch(apply, new ReactEditText(a, b));
         }
      }
      else if (isWidget(b))
      {
         if (!isWidget(a))
            applyClear = true;
         apply = appendPatch(apply, new ReactEditWidget(a, b));
      }
      if (apply != null)
         patch.put(index, apply);
      if (applyClear)
         clearState(a, patch, index);
   }

   public static List<ReactEdit> diffChildren(PrologNode a, PrologNode b, PatchSet patch, List<ReactEdit> apply, int index)
   {
      List<PrologNode> aChildren = a.getChildren();
      OrderedSet orderedSet = reorder(aChildren, b.getChildren());
      Iterator<PrologNode> bChildren = orderedSet.iterator();
      int aLen = aChildren.size();
      int bLen = orderedSet.size();
      int len = aLen > bLen ? aLen : bLen;
      for (int i = 0; i < len; i++)
      {
         PrologNode leftNode = aChildren.get(i);
         PrologNode rightNode = null;
         // We may have exhausted bChildren since len is max(aLen, bLen)
         if (bChildren.hasNext())
            rightNode = bChildren.next();
         index++;
         if (leftNode == null)
         {
            if (rightNode != null)
            {
               // Exccess nodes in b need to be added
               apply = appendPatch(apply, new ReactEditInsert(rightNode));
            }
         }
         else
         {
            walk(leftNode, rightNode, patch, index);
         }
         // FIXME: What is THIS all about?
         if ((leftNode instanceof PrologElement) && ((PrologElement)leftNode).getAttribute("count").length() > 0)
         {
            index += Integer.parseInt(((PrologElement)leftNode).getAttribute("count")); // ??
         }
      }

      Moves moves = orderedSet.moves;
      if (moves != null)
      {
         apply = appendPatch(apply, new ReactEditOrder(a, moves));
      }
      return apply;
   }
   
   private static KeyIndex keyIndex(List<PrologNode> children)
   {
      HashMap<String,Integer> keys = new HashMap<String, Integer>();
      LinkedList<Integer> free = new LinkedList<Integer>();
      int index = 0;
      for (Iterator<PrologNode> i = children.iterator(); i.hasNext();)
      {
         PrologNode child = i.next();
         String key = child.getKey();
         if (key != null)
            keys.put(key, index);
         else
            free.push(index);
         index++;
      }
      return new KeyIndex(keys, free);
   }
   
   private static OrderedSet reorder(List<PrologNode> aChildren, List<PrologNode> bChildren)
   {
      KeyIndex bChildIndex = keyIndex(bChildren);
      HashMap<String,Integer> bKeys = bChildIndex.keys;
      LinkedList<Integer> bFree = bChildIndex.free;
      if (bFree.size() == bChildren.size())
         return new OrderedSet(bChildren, null);

      KeyIndex aChildIndex = keyIndex(aChildren);
      HashMap<String,Integer> aKeys = aChildIndex.keys;
      LinkedList<Integer> aFree = aChildIndex.free;

      if (aFree.size() == aChildren.size())
         return new OrderedSet(bChildren, null);

      LinkedList<PrologNode> newChildren = new LinkedList<PrologNode>();
      int freeIndex = 0;
      int freeCount = bFree.size();
      int deletedItems = 0;

      for (int i = 0; i < aChildren.size(); i++)
      {
         PrologNode aItem = aChildren.get(i);
         Integer itemIndex;
         String aKey = aItem.getKey();
         // .push() in javascript adds to the end of the array
         if (aKey != null)
         {
            if (bKeys.containsKey(aKey))
            {
               itemIndex = bKeys.get(aKey);
               newChildren.add(bChildren.get(itemIndex.intValue()));
            }
            else
            {
               itemIndex = i - deletedItems++;
               newChildren.add(null);
            }
         }
         else
         {
            if (freeIndex < freeCount)
            {
               itemIndex = bFree.get(freeIndex++);
               newChildren.add(bChildren.get(itemIndex.intValue()));
            }
            else
            {
               itemIndex = i - deletedItems++;
               newChildren.add(null);
            }
         }
      }

      int lastFreeIndex = freeIndex >= bFree.size()?bChildren.size():bFree.get(freeIndex);
      for (int j = 0; j < bChildren.size(); j++)
      {
         PrologNode newItem = bChildren.get(j);
         String newKey = newItem.getKey();
         if (newKey != null)
         {
            if (!aKeys.containsKey(newKey))
               newChildren.push(newItem);
         }
         else if (j >= lastFreeIndex)
            newChildren.push(newItem);
      }

      List<PrologNode> simulate = new LinkedList<PrologNode>();
      // FIXME: Does this clone do what we want? We need a deep-ish clone, I think...
      simulate.addAll(newChildren.subList(0, newChildren.size()));
      int simulateIndex = 0;
      LinkedList<Remove> removes = new LinkedList<Remove>();
      LinkedList<Insert> inserts = new LinkedList<Insert>();
      PrologNode simulateItem;
      for (int k = 0; k < bChildren.size(); )
      {
         PrologNode wantedItem = bChildren.get(k);
         simulateItem = simulate.get(simulateIndex);

         while (simulateItem == null && simulate.size() > 0)
         {
            removes.push(remove(simulate, simulateIndex, null));
            simulateItem = simulate.get(simulateIndex);
         }

         if (simulateItem == null || simulateItem.getKey() != wantedItem.getKey())
         {
            String wantedItemKey = wantedItem.getKey();
            if (wantedItemKey != null)
            {
               if (simulateItem != null && simulateItem.getKey() != null)
               {
                  if (bKeys.get(simulateItem.getKey()) != k+1)
                  {
                     removes.push(remove(simulate, simulateIndex, simulateItem.getKey()));
                     simulateItem = simulate.get(simulateIndex);
                     if (simulateItem == null || simulateItem.getKey() != wantedItem.getKey())
                     {
                        inserts.push(new Insert(wantedItem.getKey(), k));
                     }
                     else
                     {
                        simulateIndex++;
                     }
                  }
                  else
                  {
                     inserts.push(new Insert(wantedItem.getKey(), k));
                  }
               }
               else
               {
                  inserts.push(new Insert(wantedItem.getKey(), k));
               }
               k++;
            }
            else if (simulateItem != null && simulateItem.getKey() != null)
            {
               removes.push(remove(simulate, simulateIndex, simulateItem.getKey()));
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
            simulateItemKey = simulateItem.getKey();
         removes.push(remove(simulate, simulateIndex, simulateItemKey));
      }

      if (removes.size() == deletedItems && inserts.size() == 0)
         return new OrderedSet(newChildren, null);
      return new OrderedSet(newChildren, new Moves(removes, inserts));
      
   }

   public static Remove remove(List<PrologNode> arr, int index, String key)
   {
      arr.remove(arr.get(index));
      return new Remove(index, key);
   }

   
   private static class OrderedSet extends LinkedList<PrologNode>
   {
      public Moves moves;
      public OrderedSet(List<PrologNode> children, Moves moves)
      {
         addAll(children);
         this.moves = moves;
      }

   }
   public static class Moves
   {
      public List<Remove> removes;
      public List<Insert> inserts;
      public Moves(List<Remove> removes, List<Insert> inserts)
      {
         this.removes = removes;
         this.inserts = inserts;
      }
   }

   public static class Remove
   {
      public int index;
      public String key;
      public Remove(int index, String key)
      {
         this.index = index;
         this.key = key;
      }
   }
   
   public static class Insert
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
}
