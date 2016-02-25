import java.util.*;
import java.net.*;
import java.io.*;

public class ServerConnection extends Thread
{
   private static Map<String, ServerConnection> connections = new HashMap<String, ServerConnection>();
   public static synchronized ServerConnection getServerConnection(String URL) throws IOException
   {
      ServerConnection connection = connections.get(URL);
      if (connection == null)
      {
         connection = new ServerConnection(URL);
         connections.put(URL, connection);
      }
      return connection;
   }

   private String URL;
   private URLConnection connection;
   private BufferedReader input;
   private ServerConnection(String URL) throws IOException
   {
      this.URL = URL;
      reconnect();
      start();
   }

   private void reconnect() throws IOException
   {
      connection = new URL(URL + "/listen").openConnection();
      input = new BufferedReader(new InputStreamReader(connection.getInputStream()));
   }

   public void run()
   {
      // FIXME: Handle disconnect
      try
      {
         while (true)
         {
            String key = input.readLine();
            List<CodeChangeListener> listeners = allListeners.get(key);
            if (listeners != null)
            {
               for (CodeChangeListener listener : listeners)
                  listener.handleCodeChange();
            }
         }
         
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
      
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
