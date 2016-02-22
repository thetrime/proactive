//https://github.com/Matt-Esch/virtual-dom


import java.io.*;
import java.util.*;
import javax.swing.*;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Component;
import java.awt.event.*;
import java.lang.reflect.*;
import java.lang.reflect.*;
import javax.xml.parsers.*;

public class React extends JFrame
{
   static PrologDocument nextDocument = null;
   static Engine engine = null;
   public static void main(String[] args) throws Exception
   {
//      Document baseDocument = builder.parse(new FileInputStream(args[0]));
      React r = new React();
//      r.setVirtualDOM(baseDocument);
//      nextDocument = builder.parse(new FileInputStream(args[1]));
      engine = new Engine();
      PrologDocument newDoc = engine.render("Element", null);
      r.setVirtualDOM(newDoc);
   }

   private ReactComponent state = null;
   private PrologNode vState = null;
   private LinkedList<PatchSet> dispatchQueue = new LinkedList<PatchSet>();
   public React() throws Exception
   {
      super("React Test");
      vState = new PrologDocument();

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

   public synchronized void setVirtualDOM(PrologDocument newState) throws Exception
   {
      updateState(newState);
      vState = newState;
   }
      
   private void updateState(PrologDocument newState)
   {
      // Compute diffs between prevState and currentState, putting them in a queue
      PatchSet editScript = ReactDiff.diff((PrologDocument)vState, newState);
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
         constructorHash.put("Panel", (Constructor)Panel.class.getConstructor(PrologNode.class));
         constructorHash.put("Field", (Constructor)Field.class.getConstructor(PrologNode.class));
         constructorHash.put("Button", (Constructor)Button.class.getConstructor(PrologNode.class));
         constructorHash.put("Title", (Constructor)Title.class.getConstructor(PrologNode.class));
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
   }

   public static ReactComponent instantiateNode(PrologNode n)
   {
      try
      {
         System.out.println("Constructing from vNode " + n);
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
      System.out.println("Unhandled type: " + n);
      System.exit(-1);
      return null;
   }

   public static void applyNodeAttributes(PrologNode n, ReactComponent target)
   {
      Map<String, Object> attributes = n.getAttributes();
      for (Iterator<Map.Entry<String, Object>> i = attributes.entrySet().iterator(); i.hasNext();)
      {
         Map.Entry<String, Object> entry = i.next();
         target.setProperty(entry.getKey(), entry.getValue());
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

