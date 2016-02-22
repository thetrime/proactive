//https://github.com/Matt-Esch/virtual-dom


import java.io.*;
import java.util.*;
import javax.swing.*;
import org.w3c.dom.*;
import javax.xml.parsers.*;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Component;
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
      Document baseDocument = builder.parse(new FileInputStream(args[0]));
      React r = new React();
      r.setVirtualDOM(baseDocument);
      nextDocument = builder.parse(new FileInputStream(args[1]));
      //System.out.println(ZhangShasha.ZhangShasha(baseDocument, nextDocument)); System.exit(-1);
   }

   private Node state = null;
   private Node vState = null;
   private LinkedList<PatchSet> dispatchQueue = new LinkedList<PatchSet>();
   public React() throws Exception
   {
      super("React Test");
      state = builder.newDocument();
      JPanel form = new Panel();
      getContentPane().setLayout(new BorderLayout());
      getContentPane().add(form, BorderLayout.CENTER);
      state.setUserData("dom", form, null);
      vState = state;
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
      vState = newState;
   }
      
   private void updateState(Document newState)
   {
      // Compute diffs between prevState and currentState, putting them in a queue
      PatchSet editScript = ReactDiff.diff((Document)vState, newState);
      queueDiffs(editScript);
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
   
   

   private void flushQueue()
   {
      LinkedList<PatchSet> queue = null;
      synchronized(dispatchQueue)
      {
         queue = dispatchQueue;
         dispatchQueue = new LinkedList<PatchSet>();
      }
      PatchSet p;
      while ((p = queue.poll()) != null)
      {
         state = p.apply(state);
      }
      System.out.println("Flushed : " + SwingUtilities.isEventDispatchThread());
   }

   private void queueDiffs(PatchSet script)
   {
      synchronized(dispatchQueue)
      {
         System.out.println("Received the following edit script: " + script);
         dispatchQueue.offer(script);
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

   public static Node instantiateNode(Node n)
   {
      try
      {
         Constructor<ReactComponent> c = constructorHash.get(n.getNodeName());
         if (c != null)
         {
            ReactComponent component = c.newInstance(n);
            n.setUserData("dom", component, null);
            return n;
         }
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }    
      System.out.println("Unhandled type: " + n.getNodeName());
      return null;
   }
}

