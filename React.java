//https://github.com/Matt-Esch/virtual-dom

import java.util.*;
import javax.swing.*;
import java.awt.BorderLayout;

public class React extends JFrame
{
   // Prevent instantiation
   private React() {}
   
   static PrologDocument nextDocument = null;
   public static void main(String[] args) throws Exception
   {
      ReactApp app = new ReactApp("http://localhost:8080/react", "App");
      app.setSize(800, 600);
      app.setDefaultCloseOperation(EXIT_ON_CLOSE);      
      app.setVisible(true);
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
   
   // this is just a convenience method
   // FIXME: It might also be wrong, since fillSpec might be a term?
   public static int getFill(Object fillSpec)
   {
      if (fillSpec instanceof String)
      {
         String fill = (String)fillSpec;
         if (fill.equals("horizontal"))
            return java.awt.GridBagConstraints.HORIZONTAL;
         else if (fill.equals("vertical"))
            return java.awt.GridBagConstraints.VERTICAL;
         else if (fill.equals("both"))
            return java.awt.GridBagConstraints.BOTH;
      }
      return java.awt.GridBagConstraints.NONE;      
   }

   public static void addCodeChangeListener(String url, String rootElementId, CodeChangeListener listener) throws Exception
   {
      ServerConnection connection = ServerConnection.getServerConnection(url);
      connection.addCodeChangeListener(rootElementId, listener);
   }
}

