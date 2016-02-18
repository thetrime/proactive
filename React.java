//https://github.com/Matt-Esch/virtual-dom


import java.io.*;
import java.util.*;
import javax.swing.*;
import org.w3c.dom.*;
import javax.xml.parsers.*;
import java.awt.*;
import java.awt.event.*;
import java.lang.reflect.*;
import java.lang.reflect.*;

public class React extends JFrame
{
   static DocumentBuilder builder;
   static Document nextDocument = null;
   public static void main(String[] args) throws Exception
   {
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      builder = factory.newDocumentBuilder();
      React r = new React();
      r.setVirtualDOM(builder.parse(new FileInputStream(args[0])));
      nextDocument = builder.parse(new FileInputStream(args[1]));
   }

   private Document state = null;
   private LinkedList<Diff> dispatchQueue = new LinkedList<Diff>();
   public React() throws Exception
   {
      super("React Test");
      state = builder.newDocument();
      JPanel form = new JPanel();
      getContentPane().setLayout(new BorderLayout());
      getContentPane().add(form, BorderLayout.CENTER);
      state.setUserData("dom", form, null);
      setSize(800, 600);
      setDefaultCloseOperation(EXIT_ON_CLOSE);      
      setVisible(true);
      JButton button = new JButton("Apply a change");
      button.addActionListener(new ActionListener()
         {
            public void actionPerformed(ActionEvent ae)
            {
               try
               {
                  setVirtualDOM(nextDocument);
               }
               catch(Exception e)
               {
                  e.printStackTrace();
               }
            }
         });
      getContentPane().add(button, BorderLayout.SOUTH);
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
      computeDiffs("", left, right, state);
      // Some other thread should flush that queue every second (or whatever seems appropriate)
      //     but for now we will request it to happen after each updateState() call
      SwingUtilities.invokeLater(new Runnable()
         {
            public void run()
            {
               flushQueue();
               validate();
            }
         });
   }


   // Note: This is not actually implemented properly! For a start we need to sort the nodes according to something so we can diff efficiently
   // Then, as a second step, we need to check for reordering
   private void computeDiffs(String indent, Node left, Node right, Node context)
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
                  computeDiffs(indent + "   ", leftChild, rightChild, left);
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
      LinkedList<Diff> queue = null;
      synchronized(dispatchQueue)
      {
         queue = dispatchQueue;
         dispatchQueue = new LinkedList<Diff>();
      }
      Diff d;
      while ((d = queue.poll()) != null)
      {
         System.out.println(d);
         d.apply();
      }
      System.out.println("Flushed : " + SwingUtilities.isEventDispatchThread());
   }

   private void queueDiff(Diff diff)
   {
      synchronized(dispatchQueue)
      {
         dispatchQueue.offer(diff);
      }
   }

   private static HashMap<String, Constructor<ReactComponent>> constructorHash = new HashMap<String, Constructor<ReactComponent>>();
   static
   {
      try
      {
         constructorHash.put("Panel", (Constructor)Panel.class.getConstructor(Node.class));
         constructorHash.put("Field", (Constructor)Field.class.getConstructor(Node.class));
         constructorHash.put("Button", (Constructor)Button.class.getConstructor(Node.class));
         constructorHash.put("Title", (Constructor)Title.class.getConstructor(Node.class));
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
   }

   public static ReactComponent instantiateNode(Node n)
   {
      try
      {
         Constructor<ReactComponent> c = constructorHash.get(n.getNodeName());
         if (c != null)         
            return c.newInstance(n);
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }    
      System.out.println("Unhandled type: " + n.getNodeName());
      return null;
   }


   private class Diff
   {
      public static final int REMOVE = 0;
      public static final int ADD = 1;

      private int action;
      private Node node;
      private Node parent;
      public Diff(int action, Node node, Node parent)
      {
         System.out.println("Diff: " + node);
         this.parent = parent;
         this.action = action;
         this.node = node;
      }
      public String toString()
      {
         if (this.action == REMOVE)
            return "Delete " + node + " from " + parent;
         else if (this.action == ADD)
            return "Add " + node + " to " + parent;
         return "??";
      }

      public void apply()
      {
         switch(action)
         {
            case REMOVE:
            {
               ReactComponent c = (ReactComponent)node.getUserData("dom");
               ((ReactComponent)parent.getUserData("dom")).removeChild(c);
               break;
            }
            case ADD:
            {
               ReactComponent c = instantiateNode(node);
               ((Container)(parent.getUserData("dom"))).add((Component)c);
               node.setUserData("dom", c, null);
               break;
            }
         }
      }
   }
   
}

