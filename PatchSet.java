import java.util.HashMap;
import java.util.List;
import org.w3c.dom.Node;

public class PatchSet extends HashMap<Integer, List<ReactEdit>>
{
   Node a;
   public PatchSet(Node a)
   {
      this.a = a;
   }
}
