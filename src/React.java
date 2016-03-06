package org.proactive;

//https://github.com/Matt-Esch/virtual-dom

import java.util.List;
import java.util.LinkedList;
import javax.swing.SwingUtilities;
import java.awt.BorderLayout;
import java.net.URI;
import org.proactive.vdom.PrologDocument;
import org.proactive.vdom.PatchSet;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.Engine;
import gnu.prolog.term.Term;

public class React
{
   // Prevent instantiation
   private React() {}

   static PrologDocument nextDocument = null;
   public static void main(String[] args) throws Exception
   {
      if (args.length != 2)
      {
         System.err.println("Usage: React <URI> <Component>");
      }
      try
      {
         long startTime = System.currentTimeMillis();
         new org.proactive.ui.ReactApp(args[0], args[1]);
         System.out.println("Render time: " + (System.currentTimeMillis() - startTime ) + "ms");
      }
      catch(Exception e)
      {
         e.printStackTrace();
         System.exit(-1);
      }
   }

   
   private static LinkedList<TreePatch> dispatchQueue = new LinkedList<TreePatch>();   

   public static void queuePatch(Term p, ReactComponent root, Engine engine)
   {
      synchronized(dispatchQueue)
      {
         System.out.println("Received patch: " + p + " for root " + root);
         if (root == null)
            throw new NullPointerException();
         dispatchQueue.offer(new TreePatch(p, root, engine));
      }
      flushQueue();
   }

   private static class TreePatch
   {
      private Term patch;
      private ReactComponent root;
      private Engine engine;
      public TreePatch(Term patch, ReactComponent root, Engine engine)
      {
         this.patch = patch;
         this.root = root;
         this.engine = engine;
      }
      public void apply() throws Exception
      {
         root = engine.applyPatch(patch, root);
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
    

   public static void addCodeChangeListener(URI uri, String rootElementId, CodeChangeListener listener) throws Exception
   {
      ServerConnection.addCodeChangeListener(uri, rootElementId, listener);
   }

}

