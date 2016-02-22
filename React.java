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
   }

   private ReactComponent state = null;
   private Node vState = null;
   private LinkedList<PatchSet> dispatchQueue = new LinkedList<PatchSet>();
   public React() throws Exception
   {
      super("React Test");
      vState = builder.newDocument();

      // Start with a real DOM that looks like <Panel/>
      // Note that we have to put the Panel *in* something - the Document object
      // In our case the Document will ALSO be a Panel. This is immutable
      
      Panel domRoot = new Panel("document");
      domRoot.setOwnerDocument(domRoot);
      domRoot.setBackground(java.awt.Color.GREEN);
      getContentPane().setLayout(new BorderLayout());
      getContentPane().add(domRoot, BorderLayout.CENTER);

      state = new Panel("default node");
      domRoot.insertChildBefore(state, null);

      
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
               repaint();
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

   public static ReactComponent instantiateNode(Node n)
   {
      try
      {
         Constructor<ReactComponent> c = constructorHash.get(n.getNodeName());
         if (c != null)
         {
            System.out.println("Constructing from vNode " + n);
            ReactComponent instance = c.newInstance(n);
            applyNodeAttributes(n, instance);
            return instance;
         }
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }    
      System.out.println("Unhandled type: " + n.getNodeName());
      return null;
   }

   public static void applyNodeAttributes(Node n, ReactComponent target)
   {
      NamedNodeMap attributes = n.getAttributes();
      for (int i = 0; i < attributes.getLength(); i++)
      {
         Attr attr = (Attr)(attributes.item(i));
         target.setProperty(attr.getName(), attr.getValue());
      }
   }
   
   public static int getFill(Object fill)
   {
      if (fill.equals("horizontal"))
         return java.awt.GridBagConstraints.HORIZONTAL;
      else if (fill.equals("vertical"))
         return java.awt.GridBagConstraints.VERTICAL;
      else if (fill.equals("both"))
         return java.awt.GridBagConstraints.BOTH;
      return java.awt.GridBagConstraints.NONE;
   }
}

