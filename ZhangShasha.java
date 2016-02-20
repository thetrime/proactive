import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.util.ArrayList;
import  java.util.List;
import java.util.Iterator;
import org.w3c.dom.*;

// This implements the Zhang-Shasha tree edit algorithm
// The original code was based on code by Aleksei Ilchenko https://github.com/ijkilchenko/ZhangShasha.git (MIT license)
//   but now bears almost no resemblance to it, except for the helper methods at the start
// I used https://fedcsis.org/proceedings/2012/pliks/222.pdf to do the actual implementation, including the code to generate the edit script

public class ZhangShasha
{ 
   public static List<Node> traverse(Node root) 
   {
      // Prepare the ordered list of nodes
      return traverse(root, new ArrayList<Node>());
   }

   private static List<Node> traverse(Node node, List<Node> nodes) 
   {
      for (int i = 0; i < node.getChildNodes().getLength(); i++) 
         nodes = traverse(node.getChildNodes().item(i), nodes);
      nodes.add(node);
      return nodes;
   }

   private static int numberNodes(Node node, int nextIndex) 
   {
      for (int i = 0; i < node.getChildNodes().getLength(); i++) 
      {
         nextIndex = numberNodes(node.getChildNodes().item(i), nextIndex);
      }
      nextIndex++;
      node.setUserData("index", nextIndex, null);
      return nextIndex;
   }

   public static List<Integer> l(Node root) 
   {
      // Cache solutions to l()
      leftmost(root);
      return l(root, new ArrayList<Integer>());
   }

   private static List<Integer> l(Node node, List<Integer> l) 
   {
      for (int i = 0; i < node.getChildNodes().getLength(); i++) 
      {
         l = l(node.getChildNodes().item(i), l);
      }
      l.add((Integer)(((Node)node.getUserData("leftmost")).getUserData("index")));
      return l;
   }

   private static void leftmost(Node node) 
   {
      if (node == null)
         return;
      for (int i = 0; i < node.getChildNodes().getLength(); i++) 
      {
         leftmost(node.getChildNodes().item(i));
      }
      if (node.getChildNodes().getLength() == 0) 
      {
         node.setUserData("leftmost", node, null);
      } else 
      {
         node.setUserData("leftmost", node.getChildNodes().item(0).getUserData("leftmost"), null);
      }
   }

   public static List<Integer> keyroots(List<Integer> l) 
   {
      List<Integer> keyroots = new ArrayList<Integer>();
      for (int i = 0; i < l.size(); i++) 
      {
         int flag = 0;
         for (int j = i + 1; j < l.size(); j++) 
         {
            if (l.get(j) == l.get(i)) 
            {
               flag = 1;
            }
         }
         if (flag == 0) 
         {
            keyroots.add(i + 1);
         }
      }
      return keyroots;
   }

   public static class Edit
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
         if (op == INSERT)
            return "Insert " + left;
         else if (op == DELETE)
            return "Delete " + left;
         else
            return "Transform " + left + " into " + right;
      }
   }

   public static int min3(int a, int b, int c)
   {
      if (a < b && a < c)
         return a;
      if (b < c)
         return b;
      return c;
   }

   public static List<Edit> editScript(String script, List<Node> left, List<Node> right)
   {
      List<Edit> list = new ArrayList<Edit>();
      Iterator<Node> i = left.iterator();
      Iterator<Node> j = right.iterator();
      int n = script.length();

      System.out.println(script);
      for (int x = 0; x < n; x++)
      {         
         char c = script.charAt(x);
         switch(c)
         {
            case 'm': // Match
               if (!nodesAreEqual(j.next(), i.next()))
               {
                  System.out.println("The edit script contains an error :(");
                  System.exit(-1);
               }
               break;
            case 'x': // Transform
               list.add(new Edit(Edit.TRANSFORM, i.next(), j.next()));
               break;
            case 'i': // Insert
               list.add(new Edit(Edit.INSERT, j.next(), null));
               break;
            case 'd': // Delete
               list.add(new Edit(Edit.DELETE, i.next(), null));
               break;
         }
      }
      return list;
   }

   public static boolean nodesAreEqual(Node node1, Node node2)
   {
      if (node1 instanceof Text && node2 instanceof Text)
         return ((Text)node1).getWholeText().equals(((Text)node2).getWholeText());
      return node1.getNodeName().equals(node2.getNodeName());
   }
   
   public static List<Edit> ZhangShasha(Node root1, Node root2) 
   {
      numberNodes(root1, 0);
      List<Integer> l1 = l(root1);
      List<Integer> K1 = keyroots(l1);
      List<Node> traversal1 = traverse(root1);
      numberNodes(root2, 0);
      List<Integer> l2 = l(root2);
      List<Integer> K2 = keyroots(l2);
      List<Node> traversal2 = traverse(root2);
      int Delete = 1;
      int Insert = 1;
      int Relabel = 1;

      
      // D[i,j] is the distance between two nodes T1[i] and T2[j]
      int[][] D = new int[l1.size() + 1][l2.size() + 1];
      String[][] DPATH = new String[l1.size() + 1][l2.size() + 1];

      // FD[T1[i,i1],T2[j,j1]] is the distance from nodes i..i1 in T1 to nodes j..j1 in T2. If i<i1 then T1[i,i1] = 0
      // Note that FD may not always need to grow quite this big
      int[][] FD = new int[K1.get(K1.size()-1) + 1][K2.get(K2.size()-1) + 1];
      String[][] PATH = new String[K1.get(K1.size()-1) + 1][K2.get(K2.size()-1) + 1];
      

      for (int x = 0; x < K1.size(); x++)      // For x = each keyroot in T1
      {
         for (int y = 0; y < K2.size(); y++)   // For y = each keyroot in T2
         {
            int k1 = K1.get(x);
            int k2 = K2.get(y);
            // FD is reset here to a zero array of dimension [x, y]. In practise we only have to zero the top-left corners since the rest is overwritten
            // First we fill in the edges of the FD array
            FD[0][l2.get(k2-1)-1] = 0;
            FD[l1.get(k1-1)-1][0] = 0;
            PATH[0][0] = "";
            for (int i = l1.get(k1-1); i <= k1; i++)
            {  // Left edge is the old tree
               FD[i][0] = FD[i-1][0] + Delete;
               PATH[i][0] = PATH[i-1][0] + "d";
            }
            for (int j = l2.get(k2-1); j <= k2; j++)
            {  // Top edge is the new tree
               FD[0][j] = FD[0][j-1] + Insert;
               PATH[0][j] = PATH[0][j-1] + "i";
            }
            // Then we fill in the rest of the FD array to compute the distance between T1[x] and T2[y]
            // Remember that l(t) is the leftmost leaf descendant of the subtree _which is rooted at t_
            // That means to find l() for a given node n, we first find where it is rooted (ie K.get(n))
            // and then find l of that (ie l(K.get(n)))
            for (int i = l1.get(k1-1); i <= k1; i++)    // i ranges from l1(k1) to k1
            {
               for (int j = l2.get(k2-1); j <= k2; j++) // j ranges from l2(k2) to k2
               {
                  int ii = (l1.get(k1 - 1) > i - 1) ? 0 : i - 1;
                  int jj = (l2.get(k2 - 1) > j - 1) ? 0 : j - 1;
                  if ((l1.get(i - 1) == l1.get(k1 - 1)) && (l2.get(j - 1) == l2.get(k2 - 1)))
                  {
                     int cost = nodesAreEqual(traversal1.get(i-1), traversal2.get(j-1))?0:Relabel;
                     FD[i][j] = min3(FD[ii][j] + Delete,
                                     FD[i][jj] + Insert,
                                     FD[ii][jj]+ cost);
                     if (FD[i][j] == FD[ii][j] + Delete)
                        PATH[i][j] = PATH[ii][j] + "d";
                     else if (FD[i][j] == FD[i][jj] + Insert)
                        PATH[i][j] = PATH[i][jj] + "i";
                     else if (cost == 0)
                        PATH[i][j] = PATH[ii][jj] + "m";
                     else
                        PATH[i][j] = PATH[ii][jj] + "x";
                     D[i][j] = FD[i][j];
                     DPATH[i][j] = PATH[i][j];
                  }
                  else
                  {
                     int i2 = l1.get(i - 1) - 1;
                     int j2 = l2.get(j - 1) - 1;
                     int iii = (l1.get(k1 - 1) > i2) ? 0 : i2;
                     int jjj = (l2.get(k2 - 1) > j2) ? 0 : j2;
                     FD[i][j] = min3(FD[ii][j]   + Delete,
                                     FD[i][jj]   + Insert,
                                     FD[iii][jjj]+ D[i][j]);
                     if (FD[i][j] == FD[ii][j] + Delete)
                        PATH[i][j] = PATH[ii][j] + "d";
                     else if (FD[i][j] == FD[i][jj] + Insert)
                        PATH[i][j] = PATH[i][jj] + "i";
                     else
                        PATH[i][j] = PATH[iii][jjj] + DPATH[i][j];
                  }
               }
            }
         }         
      }
      for (int i = 0 ; i < l1.size(); i ++)
      {
         for (int j = 0 ; j < l2.size(); j ++)
         {
            System.out.print(D[i][j] + " ");
         }
         System.out.println();
      }

      return editScript(PATH[l1.size()][l2.size()], traversal1, traversal2);
      //return D[l1.size()][l2.size()];
   }

}
