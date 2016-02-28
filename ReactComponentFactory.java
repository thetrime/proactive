import java.io.*;
import java.util.*;
import javax.swing.*;
import java.awt.event.*;
import java.lang.reflect.*;

public class ReactComponentFactory
{
   private static HashMap<String, Constructor<? extends ReactComponent>> constructorHash = new HashMap<String, Constructor<? extends ReactComponent>>();
   static
   {
      try
      {
         constructorHash.put("Panel", Panel.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("Field", Field.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("Button", Button.class.getConstructor(PrologNode.class, PrologContext.class));
         constructorHash.put("Title", Title.class.getConstructor(PrologNode.class, PrologContext.class));
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
   }

   public static ReactComponent instantiateNode(PrologNode n, PrologContext context) throws Exception
   {
      try
      {
         Constructor<? extends ReactComponent> c = constructorHash.get(n.getNodeName());
         if (c != null)
         {
            ReactComponent instance = c.newInstance(n, context);
            if (context != null)
               context.setRoot(instance);
            applyNodeAttributes(n, instance);
            return instance;
         }
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
      // User-defined component
      PrologState initialState = context.getEngine().getInitialState(n.getNodeName());
      PrologDocument userComponent = context.getEngine().render(n.getNodeName(), initialState, context.getEngine().instantiateProps(n.getAttributes()));
      if (userComponent == null)
      {
         System.out.println("Unhandled type: " + n);
         System.exit(-1);
      }
      return instantiateNode(userComponent, userComponent.getContext());
   }
   
   private static void applyNodeAttributes(PrologNode n, ReactComponent target)
   {
      Map<String, Object> attributes = n.getAttributes();
      for (Iterator<Map.Entry<String, Object>> i = attributes.entrySet().iterator(); i.hasNext();)
      {
         Map.Entry<String, Object> entry = i.next();
         target.setProperty(entry.getKey(), entry.getValue());
      }
   }
}
