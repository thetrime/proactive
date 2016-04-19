package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import org.proactive.React;
import java.awt.Component;
import javax.swing.JTextField;
import javax.swing.text.DocumentFilter;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import java.util.HashMap;
import javax.swing.InputVerifier;
import javax.swing.JComponent;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import java.awt.BorderLayout;

public class TextField implements InputWidget
{
   JTextField field = new JTextField();
   JComponent component = null;
   private DocumentFilter documentFilter = null;
   private DocumentFilter.FilterBypass bypass = null;
   private enum BypassType
   {
      INSERT, REMOVE, REPLACE;
   }
   private BypassType bypass_op;
   private int bypass_off = 0;
   private int bypass_len = 0;
   private int bypass_replacement_len = 0;
   public TextField()
   {
      if ("Mac OS X".equals(React.currentLaf))
      {
         component = field;
      }
      else
      {
         JPanel panel = new JPanel();
         panel.setLayout(new BorderLayout());
         panel.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
         panel.add(field, BorderLayout.CENTER);
         panel.setOpaque(false);
         component = panel;
      }
   }

   public Component getAWTComponent()
   {
      return component;
   }

   public Object getValue()
   {
      String text = field.getText();
      if (text.length() == 0)
         return null;
      return text;
   }

   private boolean isSystemOriginatedEvent = false;
   public void setValue(PrologObject value)
   {
      if (bypass != null)
      {
         // This setValue is happening because the user edited the field, an onChange was fired, and now is setting the
         // field to contain some new computed value. We must use bypass to set it
         String text = value.asString();
         applyText(text, bypass);
         return;
      }
      try
      {
         isSystemOriginatedEvent = true;
         if (value == null)
            field.setText("");
         else
            field.setText(value.asString());
      }
      finally
      {
         isSystemOriginatedEvent = false;
      }
      setDocumentFilter();
   }

   private void applyText(String text, DocumentFilter.FilterBypass bypass)
   {
      try
      {
         if (bypass_op == BypassType.INSERT)
         {
            // Simulate the insert and see if it gives the right value
            if (text.length() == field.getText().length() + bypass_len &&
                text.substring(0, bypass_off).equals(field.getText().substring(0, bypass_off)) &&
                text.substring(bypass_off+bypass_len).equals(field.getText().substring(bypass_off)))
            {
               // Yes, looks OK
               bypass.insertString(bypass_off, text.substring(bypass_off, bypass_off+bypass_len), null);
            }
            else
            {
               // No good. Just replace the whole lot
               bypass.replace(0, field.getText().length(), text, null);
            }

         }
         else if (bypass_op == BypassType.REMOVE)
         {
            if ((text.length() == field.getText().length() - bypass_len) &&
                text.substring(0, bypass_off).equals(field.getText().substring(0, bypass_off)) &&
                text.substring(bypass_off).equals(field.getText().substring(bypass_off + bypass_len)))
            {
               // Looks OK
               bypass.remove(bypass_off, bypass_len);
            }
            else
            {
               bypass.replace(0, field.getText().length(), text, null);
            }
         }
         else if (bypass_op == BypassType.REPLACE)
         {
            if ((text.length() == field.getText().length() - bypass_len + bypass_replacement_len) &&
                text.substring(0, bypass_off).equals(field.getText().substring(0, bypass_off)) &&
                text.substring(bypass_off + bypass_replacement_len).equals(field.getText().substring(bypass_off + bypass_len)))
            {
               // Looks OK
               bypass.replace(bypass_off, bypass_len, text.substring(bypass_off, bypass_off+bypass_replacement_len), null);
            }
            else
            {
               bypass.replace(0, field.getText().length(), text, null);
            }
         }
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
   }

   InputVerifier inputVerifier = null;
   public void setVerifier(InputWidgetVerifier listener)
   {
      if (listener == null)
         inputVerifier = null;
      else
         inputVerifier = new InputVerifier()
                         {
                            public boolean verify(JComponent ignored)
                            {
                               String newValue = field.getText();
                               HashMap<String, Object> properties = new HashMap<String, Object>();
                               if (newValue.length() == 0)
                                  newValue = null;
                               properties.put("value", newValue);
                               return listener.verifyValue(PrologObject.serialize(properties));
                            }
            };
      field.setInputVerifier(inputVerifier);
   }

   public void setChangeListener(InputWidgetListener listener)
   {
      if (listener == null)
         documentFilter = null;
      else
         documentFilter = new DocumentFilter()
                          {
                             public void insertString(FilterBypass fb, int offs, String str, AttributeSet a) throws BadLocationException
                             {
                                if (isSystemOriginatedEvent)
                                   fb.insertString(offs, str, a);
                                else
                                {
                                   bypass = fb;
                                   bypass_op = BypassType.INSERT;
                                   bypass_off = offs;
                                   bypass_len = str.length();
                                   try
                                   {
                                      String newValue = new StringBuilder(field.getText()).insert(offs, str).toString();
                                      HashMap<String, Object> properties = new HashMap<String, Object>();
                                      properties.put("value", newValue);
                                      listener.valueWouldChange(PrologObject.serialize(properties));
                                   }
                                   finally
                                   {
                                      bypass = null;
                                   }
                                }
                             }
                             public void remove(DocumentFilter.FilterBypass fb, int offset, int length)  throws BadLocationException
                             {
                                if (isSystemOriginatedEvent)
                                   fb.remove(offset, length);
                                else
                                {
                                   bypass = fb;
                                   bypass_op = BypassType.REMOVE;
                                   bypass_off = offset;
                                   bypass_len = length;
                                   try
                                   {
                                      String newValue = new StringBuilder(field.getText()).replace(offset, offset+length, "").toString();
                                      HashMap<String, Object> properties = new HashMap<String, Object>();
                                      properties.put("value", newValue);
                                      listener.valueWouldChange(PrologObject.serialize(properties));
                                   }
                                   finally
                                   {
                                      bypass = null;
                                   }

                                }
                             }
                             public void replace(DocumentFilter.FilterBypass fb, int offset, int length, String text, AttributeSet attrs)  throws BadLocationException
                             {
                                if (isSystemOriginatedEvent)
                                   fb.replace(offset, length, text, attrs);
                                else
                                {
                                   bypass = fb;
                                   bypass_op = BypassType.REPLACE;
                                   bypass_off = offset;
                                   bypass_len = length;
                                   bypass_replacement_len = text.length();
                                   try
                                   {
                                      String newValue = new StringBuilder(field.getText()).replace(offset, offset+length, text).toString();
                                      HashMap<String, Object> properties = new HashMap<String, Object>();
                                      properties.put("value", newValue);
                                      listener.valueWouldChange(PrologObject.serialize(properties));
                                   }
                                   finally
                                   {
                                      bypass = null;
                                   }

                                }
                             }
            };
      setDocumentFilter();
   }

   private void setDocumentFilter()
   {
      AbstractDocument document = (AbstractDocument)field.getDocument();
      document.setDocumentFilter(documentFilter);
   }

   public void setDisabled(boolean disabled)
   {
      field.setEnabled(!disabled);
   }

   public void setAlignment(int alignment)
   {
      field.setHorizontalAlignment(alignment);
   }

}
