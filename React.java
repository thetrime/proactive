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
      engine = new Engine();
      React r = new React("App");
   }

   private static LinkedList<TreePatch> dispatchQueue = new LinkedList<TreePatch>();
   public React(String rootElementId) throws Exception
   {
      super("React Test");
      // This is a bit finicky. First we have to set up the state as 'empty'.
      // The empty state is not as empty as you might think. It contains 2 nodes:
      //    * The global domRoot. This is akin to the JFrame
      //    * Inside this is a RootPanel. This is like the contentPane in the frame
      // Unlike in Swing, we can change the contentPane to a new one by patching it
      // but the global domRoot is immutable
      
      Panel domRoot = new Panel("root");
      domRoot.setOwnerDocument(domRoot);
      domRoot.setBackground(java.awt.Color.GREEN);
      getContentPane().setLayout(new BorderLayout());
      getContentPane().add(domRoot, BorderLayout.CENTER);

      ReactComponent swingTree = new RootPanel(rootElementId);
      domRoot.insertChildBefore(swingTree, null);
      swingTree.getContext().setRoot(swingTree);
      swingTree.getContext().reRender();
           
      setSize(800, 600);
      setDefaultCloseOperation(EXIT_ON_CLOSE);      
      setVisible(true);      
   }


   public static void queuePatch(PatchSet p, ReactComponent root, PrologContext context)
   {
      synchronized(dispatchQueue)
      {
         dispatchQueue.offer(new TreePatch(p, root, context));
      }
      flushQueue();
   }

   private static class TreePatch
   {
      private PatchSet patch;
      private ReactComponent root;
      private PrologContext context;
      public TreePatch(PatchSet patch, ReactComponent root, PrologContext context)
      {
         this.patch = patch;
         this.root = root;
         this.context = context;
      }
      public void apply() throws Exception
      {
         root = patch.apply(root);
         if (context != null)
            context.setRoot(root);
      }
   }

   public static void flushQueue()
   {
      SwingUtilities.invokeLater(new Runnable()
         {
            public void run()
            {
               flushQueueAsAWT();
            }
         });
   }
   

   private static void flushQueueAsAWT()
   {
      LinkedList<TreePatch> queue = null;
      synchronized(dispatchQueue)
      {
         // Swap the old queue for a new one
         queue = dispatchQueue;
         dispatchQueue = new LinkedList<TreePatch>();
      }
      TreePatch p;
      while ((p = queue.poll()) != null)
      {
         try
         {
            p.apply();
         }
         catch(Exception e)
         {
            e.printStackTrace();
         }
      }
   }

   public static int getFill(Object fillSpec)
   {
      String fill = engine.asString(fillSpec);
      if (fill.equals("horizontal"))
         return java.awt.GridBagConstraints.HORIZONTAL;
      else if (fill.equals("vertical"))
         return java.awt.GridBagConstraints.VERTICAL;
      else if (fill.equals("both"))
         return java.awt.GridBagConstraints.BOTH;
      return java.awt.GridBagConstraints.NONE;
   }
}

