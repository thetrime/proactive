package org.proactive;

import java.util.*;
import java.net.*;
import java.io.*;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.drafts.Draft_17;
import org.java_websocket.handshake.ServerHandshake;


public class ServerConnection extends WebSocketClient
{
   private static Map<URI, ServerConnection> connections = new HashMap<URI, ServerConnection>();
   public static synchronized ServerConnection getServerConnection(URI uri) throws IOException
   {
      ServerConnection connection = connections.get(uri);
      if (connection == null)
      {
         connection = new ServerConnection(uri);
         connections.put(uri, connection);
      }
      return connection;
   }

   private ServerConnection(URI URI) throws IOException
   {
      super(URI, new Draft_17());
      connect();
   }

   @Override
   public void onMessage(String key)
   {
      List<CodeChangeListener> listeners = allListeners.get(key);
      if (listeners != null)
      {
         for (CodeChangeListener listener : listeners)
            listener.handleCodeChange();
      }
   }
      
   @Override
   public void onOpen(ServerHandshake handshake)
   {
      System.out.println("opened connection");
   }
   
   @Override
   public void onClose(int code, String reason, boolean remote)
   {
      System.out.println("closed connection");
   }
      
   @Override
   public void onError(Exception ex)
   {
      ex.printStackTrace();
      // FIXME: Should reconnect here
   }           

   private Map<String, List<CodeChangeListener>> allListeners = new HashMap<String, List<CodeChangeListener>>();
   public  void addCodeChangeListener(String element, CodeChangeListener listener)
   {
      List<CodeChangeListener> listeners = allListeners.get(element);
      if (listeners == null)
      {
         listeners = new LinkedList<CodeChangeListener>();
         allListeners.put(element, listeners);
      }
      listeners.add(listener);
   }
}
