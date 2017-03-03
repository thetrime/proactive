package org.proactive.ui;

import java.net.URL;
import javax.swing.JLabel;
import javax.swing.ImageIcon;
import java.util.HashMap;
import java.awt.Dimension;
import java.awt.Component;
import org.proactive.ReactLeafComponent;
import org.proactive.ReactWidget;
import org.proactive.prolog.PrologObject;

public class Image extends ReactLeafComponent 
{
   private JLabel component;
   private int width;
   private int height;
   private String src;
   private URL url;
   private ImageIcon icon = null;
   public Image()
   {
      component = new JLabel("Broken image");
   }

   public void setProperties(HashMap<String,PrologObject> properties)
   {
      super.setProperties(properties);
      boolean mustRedraw = false;
      if (properties.containsKey("width"))
      {
         if (properties.get("width") == null)
            width = -1;
         else
            width = properties.get("width").asInteger();
         mustRedraw = true;
      }
      if (properties.containsKey("height"))
      {
         if (properties.get("height") == null)
            height = -1;
         else
            height = properties.get("height").asInteger();
         mustRedraw = true;
      }
      if (properties.containsKey("src"))
      {
         if (properties.get("src") == null)
         {
            src = null;
            url = null;
            mustRedraw = true;
         }
         else
         {
            try
            {
               src = properties.get("src").asString();
               if (src.startsWith("/"))
               {
                  if (getOwnerDocument() != null)
                     url = getOwnerDocument().getEngine().relativeURL(src);
                  else
                     url = null;
               }
               else
                  url = new URL(src);
               mustRedraw = true;
            }
            catch(Exception e)
            {
               e.printStackTrace();
               src = null;
            }
         }
      }
      if (mustRedraw)
         redrawIcon();
   }

   private void redrawIcon()
   {
      if (url == null)
      {
         icon = null;
         component.setIcon(null);
      }
      else
      {
         icon = new ImageIcon(url);
         if (width == -1)
            width = icon.getIconWidth();
         if (height == -1)
            height = icon.getIconHeight();
         component.setText(null);
         if (width != icon.getIconWidth() || height != icon.getIconHeight())
         {
            // Scale image
            icon = new ImageIcon(icon.getImage().getScaledInstance(width, height, java.awt.Image.SCALE_SMOOTH));
         }
         component.setIcon(icon);

      }
   }

   public Component getAWTComponent()
   {
      return component;
   }

   public String toString()
   {
      return "<Image: " + src + ">";
   }

   public void setOwnerDocument(ReactWidget w)
   {
      super.setOwnerDocument(w);
      if (url == null && src != null && w != null)
      {
         try
         {
            if (src.startsWith("/"))
               url = w.getEngine().relativeURL(src);
            else
               url = new URL(src);
            redrawIcon();
         }
         catch(Exception e)
         {
            e.printStackTrace();
         }
      }

   }
}
