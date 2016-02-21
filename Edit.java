import org.w3c.dom.*;

// There are 3 edit operations that Zhang Shasha defines:
//   1) Transform a node into another node
//        This is actually impossible in Java. In reality, either:
//        A) Change the attributes, if the class is the same
//        B) Change the class completely, which means deleting the old node (preserving the children!), then replacing it with a new node
//   2) Delete a node, *moving all the children up in order*
//   3) Insert a node between an existing node and a subsequence of consecutive children of this node.

public class Edit
{
   int op;
   public static final int INSERT = 0;
   public static final int DELETE = 1;
   public static final int TRANSFORM = 2;
   Node left;
   Node right;
   
   public Edit(int op, Node left, Node right)
   {
      this.op = op;
      this.left = left;
      this.right = right;
   }
   
   public String toString()
   {
      switch(op)
      {
         case INSERT:
            return "Insert " + right + " into " + left;
         case DELETE:
            return "Delete " + left;
         case TRANSFORM:
            return "Transform " + left + " into " + right;
      }
      return "<unknown operation>";
   }

   public void apply()
   {
      switch(op)
      {
         case INSERT:
         {
            ReactComponent component = React.instantiateNode(right);
            System.out.println("inserting " + right + " into " + left);
            ((ReactComponent)left.getUserData("dom")).insertChildBefore(component, null);
            break;
         }
         case DELETE:
         {
            ReactComponent component = ((ReactComponent)left.getUserData("dom"));
            Node parent = left.getParentNode();
            // FIXME: We must get all the children and insert them after this node, THEN delete this node
            ((ReactComponent)parent.getUserData("dom")).removeChild(component);
            break;
         }
         case TRANSFORM:
         {
            break;
         }
      }
   }
}

