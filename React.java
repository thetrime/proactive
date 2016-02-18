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
      updateState(state, newState);
      state = newState;
   }
   

   private void updateState(Node prevState, Node currentState)
   {
      // Compute diffs between prevState and currentState, putting them in a queue
      computeDiffs(prevState, currentState, getContentPane());
      // Some other thread should flush that queue every second (or whatever seems appropriate)
      //     but for now we will request it to happen after each updateState() call
      flushQueue();
      /*
      SwingUtilities.invokeLater(new Runnable()
         {
            public void run()
            {
               flushQueue();
               validate();
            }
         });
      */
   }


   // Note: This is not actually implemented properly!   
   private void computeDiffs(Node prevState, Node currentState, Container context)
   {
      if (prevState == currentState)
      {
         return;
      }
      if (currentState == null)
      {
         // Delete everything present in prevState
         System.out.println("Deleting because currentState is null but prevState has " + prevState);
         queueDiff(new Diff(Diff.REMOVE, prevState, context));
      }
      else if (prevState == null)
      {
         System.out.println("Adding " + currentState);
         queueDiff(new Diff(Diff.ADD, currentState, context));
      }
      else
      {
         if (prevState.getNodeName() == currentState.getNodeName())
         {
            // diffProps here. If present, we need to add patches for them
            // diffChildren:
            Node leftChild = prevState.getFirstChild();
            Node rightChild = currentState.getFirstChild();
            Container childContext = (Container)prevState.getUserData("dom");
            while (leftChild != null || rightChild != null)
            {
               computeDiffs(leftChild, rightChild, childContext);
               if (leftChild != null)
                  leftChild = leftChild.getNextSibling();               
               if (rightChild != null)
                  rightChild = rightChild.getNextSibling();            
            }
         }
         else
         {
            queueDiff(new Diff(Diff.REMOVE, prevState, context));
            queueDiff(new Diff(Diff.ADD, currentState, context));
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
               break;
            case ADD:
               ReactComponent c = instantiateNode(node);
               parent.add((Component)c);
               node.setUserData("dom", c, null);
               break;
         }
      }
   }
   
}
