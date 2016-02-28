package org.proactive;

//https://github.com/Matt-Esch/virtual-dom

import java.util.*;
import javax.swing.*;
import java.awt.BorderLayout;
import java.net.URI;
import org.proactive.vdom.PrologDocument;
import org.proactive.vdom.PatchSet;
import org.proactive.prolog.PrologContext;

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
      ReactApp app = new ReactApp(args[0], args[1]);
      JFrame frame = app.getAWTComponent();
      frame.setSize(800, 600);
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);      
      frame.setVisible(true);
   }

   
   private static LinkedList<TreePatch> dispatchQueue = new LinkedList<TreePatch>();   

   public static void queuePatch(PatchSet p, ReactComponent root, PrologContext context)
   {
      synchronized(dispatchQueue)
      {
         //System.out.println("Received patch: " + p);
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
    

   public static void addCodeChangeListener(URI uri, String rootElementId, CodeChangeListener listener) throws Exception
   {
      ServerConnection connection = ServerConnection.getServerConnection(uri);
      connection.addCodeChangeListener(rootElementId, listener);
   }

}

