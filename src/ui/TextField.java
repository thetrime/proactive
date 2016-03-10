package org.proactive.ui;

import org.proactive.prolog.PrologObject;
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

public class TextField implements InputWidget
{
   JTextField field = new JTextField();
   private DocumentFilter documentFilter = null;
   private DocumentFilter.FilterBypass bypass = null;
   public TextField()
   {
   }

   public Component getAWTComponent()
   {
      return field;
   }

   public Object getValue()
   {
      return field.getText();
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
         bypass.replace(0, field.getText().length(), text, null);
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


}
