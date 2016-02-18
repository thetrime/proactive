//https://github.com/Matt-Esch/virtual-dom

import java.io.*;
import java.util.*;
import javax.swing.*;
import org.w3c.dom.*;
import javax.xml.parsers.*;
import java.awt.*;

public class React extends JFrame
{
   static DocumentBuilder builder;
   public static void main(String[] args) throws Exception
   {
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      builder = factory.newDocumentBuilder();
      React r = new React();
      System.out.println("---------------------------- Setting " + args[0]);
      r.setVirtualDOM(builder.parse(new FileInputStream(args[0])));
      System.out.println("---------------------------- Setting " + args[1]);
      r.setVirtualDOM(builder.parse(new FileInputStream(args[1])));
      System.out.println("----------------------------");
   }

   private Document state = null;
   private LinkedList<Diff> dispatchQueue = new LinkedList<Diff>();
   public React() throws Exception
   {
      state = builder.newDocument();
      state.setUserData("dom", getContentPane(), null);
      setSize(800, 600);
      setDefaultCloseOperation(EXIT_ON_CLOSE);      
      setVisible(true);
   }

   public synchronized void setVirtualDOM(Document newState) throws Exception
   {
      updateState(newState);
      state = newState;
   }
      
   private void updateState(Node newState)
   {
      Node left = state.getFirstChild();
      Node right = newState.getFirstChild();
      // Compute diffs between prevState and currentState, putting them in a queue
      computeDiffs("", left, right, getContentPane());
      // Some other thread should flush that queue every second (or whatever seems appropriate)
      //     but for now we will request it to happen after each updateState() call
//      SwingUtilities.invokeLater(new Runnable()
//         {
//            public void run()
//            {
               flushQueue();
               validate();
//            }
//         });
   }


   // Note: This is not actually implemented properly! For a start we need to sort the nodes according to something so we can diff efficiently
   // Then, as a second step, we need to check for reordering
   private void computeDiffs(String indent, Node left, Node right, Container context)
   {
      while (true)
      {
         System.out.println(indent + "Comparing " + left + " <-> " + right);
         if (left == null && right == null)
            return;
         if (left != right)
         {
            if (right == null)
            {
               // Delete everything present in left
               System.out.println(indent + "* Deleting because right is null but left has " + left);
               queueDiff(new Diff(Diff.REMOVE, left, context));
               left = left.getNextSibling();
            }
            else if (left == null)
            {
               // Add the node in right
               System.out.println(indent + "* Adding because left is null but right has " + right);
               queueDiff(new Diff(Diff.ADD, right, context));
               right = right.getNextSibling();
            }
            else
            {
               if (left.getNodeName() == right.getNodeName())
               {
                  // diffProps here. If present, we need to add patches for them
                  // diffChildren:
                  Node leftChild = left.getFirstChild();
                  Node rightChild = right.getFirstChild();
                  Container childContext = (Container)left.getUserData("dom");
                  computeDiffs(indent + "   ", leftChild, rightChild, childContext);
                  left = left.getNextSibling();
                  right = right.getNextSibling();
               }
               else if (true)
               {
                  // This is the case if left > right
                  System.out.println(indent + "* Must delete " + left + " because of mismatch");
                  queueDiff(new Diff(Diff.REMOVE, left, context));
                  left = left.getNextSibling();
               }
               else
               {
                  // This is the case if left < right
                  System.out.println(indent + "* Must insert " + right + " because of mismatch");
                  queueDiff(new Diff(Diff.ADD, right, context));
                  right = right.getNextSibling();
               }
            }
         }
      }      
   }

   private void flushQueue()
   {
      Diff d;
      while ((d = dispatchQueue.poll()) != null)
      {
         System.out.println(d);
         d.apply();
      }
      System.out.println("Flushed : " + SwingUtilities.isEventDispatchThread());
   }

   private void queueDiff(Diff diff)
   {
      dispatchQueue.offer(diff);
   }

   public static ReactComponent instantiateNode(Node n)
   {
      if (n.getNodeName().equals("Form"))
         return new Form(n);
      if (n.getNodeName().equals("Field"))
         return new Field(n);
      if (n.getNodeName().equals("Title"))
         return new Title(n);
      if (n.getNodeName().equals("Button"))
         return new Button(n);
      System.out.println("Unhandled type: " + n.getNodeName());
      return null;
   }


   private class Diff
   {
      public static final int REMOVE = 0;
      public static final int ADD = 1;

      private int action;
      private Node node;
      private Container parent;
      public Diff(int action, Node node, Container parent)
      {
         System.out.println("Diff: " + node);
         this.parent = parent;
         this.action = action;
         this.node = node;
      }
      public String toString()
      {
         if (this.action == REMOVE)
            return "Delete " + node;
         else if (this.action == ADD)
            return "Add " + node;
         return "??";
      }

      public void apply()
      {
         switch(action)
         {
            case REMOVE:
            {
               ReactComponent c = (ReactComponent)node.getUserData("dom");
               ((ReactComponent)parent).removeChild(c);
               break;
            }
            case ADD:
            {
               ReactComponent c = instantiateNode(node);
               parent.add((Component)c);
               node.setUserData("dom", c, null);
               break;
            }
         }
      }
   }
   
}
