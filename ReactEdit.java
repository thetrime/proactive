import org.w3c.dom.*;

// There are 6 edit operations that React permits:

public class ReactEdit
{
   int op;
   public static final int NONE = 0;
   public static final int TEXT = 1;
   public static final int NODE = 2;
   public static final int WIDGET = 3;
   public static final int PROPS = 4;
   public static final int ORDER = 5;
   public static final int INSERT = 6;
   public static final int REMOVE = 7;
   public static final int THUNK = 8;
   Object left;
   Object right;
   
   public ReactEdit(int op, Object left, Object right)
   {
      this.op = op;
      this.left = left;
      this.right = right;
   }
   
   public String toString()
   {
      switch(op)
      {
      }
      return "<unknown operation " + op + ">";
   }

   public void apply()
   {
      switch(op)
      {
      }
   }
}

