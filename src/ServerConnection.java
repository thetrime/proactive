package org.proactive;

import java.util.List;
import java.util.LinkedList;
import java.util.HashMap;
import java.util.Map;
import java.net.URI;
import java.io.IOException;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.drafts.Draft_17;
import org.java_websocket.handshake.ServerHandshake;

public class ServerConnection extends WebSocketClient
{
   private static List<URI> activeConnections = new LinkedList<URI>();
   private static Map<URI, Map<String, List<CodeChangeListener>>> allListeners = new HashMap<URI, Map<String, List<CodeChangeListener>>>();   
   private static Map<String, String> emptyHTTPHeaders = new HashMap<String, String>();
   private URI uri;
   private boolean isReplacement;
   private ServerConnection(URI URI, boolean isReplacement) throws IOException
   {
      super(URI, new Draft_17());
      this.isReplacement = isReplacement;
      this.uri = URI;
      connect();
   }

   @Override
   public void onMessage(String key)
   {
      Map<String, List<CodeChangeListener>> listenersForURI = allListeners.get(uri);
      List<CodeChangeListener> listeners = listenersForURI.get(key);
      if (listeners != null)
      {
         for (CodeChangeListener listener : listeners)
            listener.handleCodeChange();
      }
   }
      
   @Override
   public void onOpen(ServerHandshake handshake)
   {
      System.out.println("opened connection for source listener");
      if (isReplacement)
      {
         // This should also trigger a change for ALL components if successful!
         for (List<CodeChangeListener> entry : allListeners.get(uri).values())
            for (CodeChangeListener listener : entry)
               listener.handleCodeChange();
      }
   }
   
   @Override
   public void onClose(int code, String reason, boolean remote)
   {
      System.out.println("closed connection: " + remote);      
      boolean restarted = false;
      while (!restarted)
      {
         try
         {
            Thread.sleep(3000);
         }
         catch (Exception e)
         {
            
         }
         try
         {
            new ServerConnection(uri, true);
            restarted = true;
         }
         catch(Exception f)
         {
         }
      }
   }
      
   @Override
   public void onError(Exception ex)
   {
      ex.printStackTrace();
   }           

   public static synchronized void addCodeChangeListener(URI uri, String element, CodeChangeListener listener) throws IOException
   {
      Map<String, List<CodeChangeListener>> class2Listeners = allListeners.get(uri);
      if (class2Listeners == null)
      {
         class2Listeners = new HashMap<String, List<CodeChangeListener>>();
         allListeners.put(uri, class2Listeners);
      }
      List<CodeChangeListener> listOfListeners = class2Listeners.get(element);
      if (listOfListeners == null)
      {
         listOfListeners = new LinkedList<CodeChangeListener>();
         class2Listeners.put(element, listOfListeners);
      }
      listOfListeners.add(listener);
      if (!activeConnections.contains(uri))
      {
         ServerConnection connection = new ServerConnection(uri, false);
         activeConnections.add(uri);
      }
   }
}
