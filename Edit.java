import org.w3c.dom.*;

// There are 3 edit operations that Zhang Shasha defines:
//   1) Transform a node into another node
//        This is actually impossible in Java. In reality, either:
//        A) Change the attributes, if the class is the same
//        B) Change the class completely, which means deleting the old node (preserving the children!), then replacing it with a new node
//   2) Delete a node, *moving all the children up*
//   3) Insert a node *somewhere* 

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
            return "Insert " + left;
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
            break;
         case DELETE:
            break;
         case TRANSFORM:
            break;
      }
   }
}

