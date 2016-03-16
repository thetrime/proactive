package org.proactive.ui;

import javax.swing.JTextField;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JCheckBox;
import javax.swing.JPasswordField;
import javax.swing.JPopupMenu;
import javax.swing.JMenuItem;
import javax.swing.BorderFactory;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import java.awt.BorderLayout;
import javax.swing.event.PopupMenuListener;
import javax.swing.event.PopupMenuEvent;
import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import org.proactive.prolog.PrologObject;
import org.proactive.ReactComponent;

public class Field extends ReactComponent
{
   private static final int TEXT = 0;
   private static final int RADIO = 1;
   private static final int CHECKBOX = 2;
   private static final int PASSWORD = 3;

   private ReactPopupMenu popup = null;
   private InputWidget widget;
   private int type = TEXT;
   public Field()
   {
      widget = new TextField();
      popup = new ReactPopupMenu();
      popup.addPopupMenuListener(new PopupMenuListener()
         {
            public void popupMenuCanceled(PopupMenuEvent e)
            {
               System.out.println("Cancel menu");
               popup.setVisible(true);
            }
            public void popupMenuWillBecomeVisible(PopupMenuEvent e)
            {
               System.out.println("Show menu");
            }
            public void popupMenuWillBecomeInvisible(PopupMenuEvent e)
            {
               System.out.println("Hide menu");
               popup.setVisible(true);
            }
         });
   }

   public class ReactPopupMenu extends JPopupMenu
   {
      public void setVisible(boolean b)
      {
         super.setVisible(b);
         if (!b)
         {
            // This is a bit ugly really. Almost always you would want to dismiss the popup at this point
            // however, we cannot rely on the event to actually hide it
            super.setVisible(true);
         }
      }
   }

   private PrologObject serializeObject()
   {
      HashMap<String, Object> properties = new HashMap<String, Object>();
      properties.put("value", getValue());
      return PrologObject.serialize(properties);
   }

   private FocusListener focusListener = null;
   private void setFocusListener(PrologObject value)
   {
      if (focusListener != null)
         widget.getAWTComponent().removeFocusListener(focusListener);
      if (value == null || value.isNull())
         return;
      focusListener = new FocusListener()
         {
            public void focusLost(FocusEvent fe)
            {
               try
               {
                  getOwnerDocument().triggerEvent(value.asTerm(), serializeObject().asTerm());
               }
               catch (Exception e)
               {
                  e.printStackTrace();
               }
            }
            public void focusGained(FocusEvent fe)
            {
            }
         };
      widget.getAWTComponent().addFocusListener(focusListener);
   }

   private MouseListener contextMenuListener = null;
   private void setContextMenuListener(PrologObject value)
   {
      if (contextMenuListener != null)
         widget.getAWTComponent().removeMouseListener(contextMenuListener);
      if (value == null || value.isNull())
         return;
      System.out.println("onContext: " + value);
      contextMenuListener = new MouseAdapter()
         {
            public void triggerPopup()
            {
               try
               {
                  getOwnerDocument().triggerEvent(value.asTerm(), serializeObject().asTerm());
               }
               catch (Exception e)
               {
                  e.printStackTrace();
               }
            }

            public void mouseClicked(MouseEvent me)
            {
               if (me.isPopupTrigger())
               {
                  triggerPopup();
               }
            }
            public void mousePressed(MouseEvent me)
            {
               if (me.isPopupTrigger())
               {
                  triggerPopup();
               }
            }
         };
      widget.getAWTComponent().addMouseListener(contextMenuListener);
   }


   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
      if (properties.containsKey("type"))
      {
         int oldType = type;
         String key = properties.get("type").asString();
         if (key == null)
            type = TEXT;
         else if (key.equals("text"))
            type = TEXT;
         else if (key.equals("radio"))
            type = RADIO;
         else if (key.equals("checkbox"))
            type = CHECKBOX;
         else if (key.equals("password"))
            type = PASSWORD;
         if (oldType != type)
         {
            switch(type)
            {
               case TEXT:
                  widget = new TextField();
                  break;
               case PASSWORD:
                  widget = new PasswordField();
                  break;
               case RADIO:
                  widget = new RadioButton();
                  break;
               case CHECKBOX:
                  widget = new CheckBox();
                  break;
            }
            if (getParentNode() != null)
               getParentNode().replaceChild(this, this);
         }
      }
      if (properties.containsKey("value"))
      {
         if (properties.get("value") == null)
            setValue(null);
         else
            setValue(properties.get("value"));
      }
      if (properties.containsKey("onBlur"))
         setFocusListener(properties.get("onBlur"));
      if (properties.containsKey("disabled"))
         widget.setDisabled(properties.get("disabled").asBoolean());

      if (properties.containsKey("verifyValue"))
      {
         PrologObject handler = properties.get("verifyValue");
         if (handler == null || handler.isNull())
            widget.setVerifier(null);
         else
            widget.setVerifier(new InputWidgetVerifier()
               {
                  public boolean verifyValue(PrologObject newValue)
                  {
                     try
                     {
                        return getOwnerDocument().triggerEvent(handler.asTerm(), newValue.asTerm());
                     }
                     catch(Exception e)
                     {
                        e.printStackTrace();
                     }
                     return false;
                  }
               });
      }
      if (properties.containsKey("onContextMenu"))
         setContextMenuListener(properties.get("onContextMenu"));
      if (properties.containsKey("onChange"))
      {
         PrologObject handler = properties.get("onChange");
         if (handler == null || handler.isNull())
            widget.setChangeListener(null);
         else
            widget.setChangeListener(new InputWidgetListener()
               {
                  public void valueWouldChange(PrologObject newValue)
                  {
                     try
                     {
                        getOwnerDocument().triggerEvent(handler.asTerm(), newValue.asTerm());
                     }
                     catch(Exception e)
                     {
                        e.printStackTrace();
                     }
                  }
               });
      }
      if (properties.containsKey("contextMenu"))
      {
         PrologObject menu = properties.get("contextMenu");
         if (menu.isNull())
         {
            popup.setVisible(false);
         }
         else
         {
            popup.removeAll();
            popup.add(new JMenuItem("Foo"));
            popup.add(new JMenuItem("Bar"));
            popup.add(new JMenuItem("Baz"));
            if (widget.getAWTComponent().isDisplayable())
               popup.show(widget.getAWTComponent(), 0, 0);
         }
      }

   }

   public class PopupMenu extends ReactComponent
   {
      public Component getAWTComponent() { return null; }
   }

   public class MenuItem extends ReactComponent
   {
      public Component getAWTComponent() { return null; }
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      super.insertChildBefore(child, sibling);
      if (child instanceof PopupMenu)
      {
         System.out.println("Got popup menu!");
         createPopupFrom((PopupMenu)child, popup);
         if (widget.getAWTComponent().isDisplayable())
            popup.show(widget.getAWTComponent(), 0, 0);
      }
   }

   public void createPopupFrom(PopupMenu item, JPopupMenu popup)
   {
   }

   public void removeChild(ReactComponent child)
   {
      super.removeChild(child);
      if (child instanceof PopupMenu)
      {
         popup.removeAll();
         popup.setVisible(false);
      }
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      if (newChild instanceof PopupMenu)
      {
         createPopupFrom((PopupMenu)newChild, popup);
         if (widget.getAWTComponent().isDisplayable())
            popup.show(widget.getAWTComponent(), 0, 0);
      }
      if (oldChild instanceof PopupMenu)
      {
         popup.removeAll();
         popup.setVisible(false);
      }
      super.replaceChild(newChild, oldChild);
   }

   public void setValue(PrologObject value)
   {
      widget.setValue(value);
   }

   public Object getValue()
   {
      return widget.getValue();
   }

   public Component getAWTComponent()
   {
      return widget.getAWTComponent();
   }
}
