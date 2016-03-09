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

public class TextField implements InputWidget
{
   JTextField field = new JTextField();
   private DocumentFilter documentFilter = null;
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
                                   String newValue = new StringBuilder(field.getText()).insert(offs, str).toString();
                                   HashMap<String, Object> properties = new HashMap<String, Object>();
                                   properties.put("value", newValue);
                                   listener.valueWouldChange(PrologObject.serialize(properties));
                                }
                             }
                             public void remove(DocumentFilter.FilterBypass fb, int offset, int length)  throws BadLocationException
                             {
                                if (isSystemOriginatedEvent)
                                   fb.remove(offset, length);
                                else
                                {
                                   String newValue = new StringBuilder(field.getText()).replace(offset, offset+length, "").toString();
                                   HashMap<String, Object> properties = new HashMap<String, Object>();
                                   properties.put("value", newValue);
                                   listener.valueWouldChange(PrologObject.serialize(properties));
                                }
                             }
                             public void replace(DocumentFilter.FilterBypass fb, int offset, int length, String text, AttributeSet attrs)  throws BadLocationException
                             {
                                if (isSystemOriginatedEvent)
                                   fb.replace(offset, length, text, attrs);
                                else
                                {
                                   String newValue = new StringBuilder(field.getText()).replace(offset, offset+length, text).toString();
                                   HashMap<String, Object> properties = new HashMap<String, Object>();
                                   properties.put("value", newValue);
                                   listener.valueWouldChange(PrologObject.serialize(properties));
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
