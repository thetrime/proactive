package org.proactive.vdom;
import org.proactive.ReactComponent;

import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.List;

public class ReactEditOrder extends ReactEdit
{
   ReactDiff.Moves moves;
   public ReactEditOrder(PrologNode node, ReactDiff.Moves moves)
   {      
      super(node);
      this.moves = moves;
   }

   public ReactComponent apply(ReactComponent domNode) throws Exception
   {
      List<ReactComponent> childNodes = domNode.getChildNodes();
      Map<String, ReactComponent> keyMap = new HashMap<String, ReactComponent>();
      for (Iterator<ReactDiff.Remove> i = moves.removes.iterator(); i.hasNext();)
      {
         ReactDiff.Remove remove = i.next();
         ReactComponent node = childNodes.get(remove.index);
         if (remove.key != null)
            keyMap.put(remove.key, node);
         domNode.removeChild(node);
      }
      int length = childNodes.size();
      for (Iterator<ReactDiff.Insert> i = moves.inserts.iterator(); i.hasNext();)
      {
         ReactDiff.Insert insert = i.next();
         ReactComponent node = keyMap.get(insert.key);
         domNode.insertChildBefore(node, insert.to >= length++ ? null : childNodes.get(insert.to));
      }
      return domNode;
   }
   
   public String toString()
   {
      return "<Reorder elements: " + node + ", " + moves + ">";
   }
}
