package org.proactive;

import java.util.HashMap;

public class StyleSheet
{
   private HashMap<String, HashMap<String, Object>> classSheet = new HashMap<String, HashMap<String, Object>>();
   private HashMap<String, HashMap<String, Object>> idSheet = new HashMap<String, HashMap<String, Object>>();
   private HashMap<String, HashMap<String, Object>> typeSheet = new HashMap<String, HashMap<String, Object>>();

   public StyleSheet()
   {
   }

   public void setValueForId(String id, String key, Object value)
   {
      HashMap<String, Object> values = idSheet.get(id);
      if (values == null)
      {
         if (value == null)
            return;
         values = new HashMap<String, Object>();
         idSheet.put(id, values);
      }
      if (value == null)
         values.remove(key);
      else
         values.put(key, value);
   }

   public void setValueForClass(String className, String key, Object value)
   {
      HashMap<String, Object> values = classSheet.get(className);
      if (values == null)
      {
         if (value == null)
            return;
         values = new HashMap<String, Object>();
         classSheet.put(className, values);
      }
      if (value == null)
         values.remove(key);
      else
         values.put(key, value);
   }

   public void setValueForType(String typeName, String key, Object value)
   {
      HashMap<String, Object> values = typeSheet.get(typeName);
      if (values == null)
      {
         if (value == null)
            return;
         values = new HashMap<String, Object>();
         typeSheet.put(typeName, values);
      }
      if (value == null)
         values.remove(key);
      else
         values.put(key, value);
   }

   public Object getValue(String id, String className, String type, String key)
   {
      HashMap<String, Object> values = idSheet.get(id);
      if (values != null && values.containsKey(key))
         return values.get(key);
      values = classSheet.get(className);
      if (values != null && values.containsKey(key))
         return values.get(key);
      values = typeSheet.get(className);
      if (values != null && values.containsKey(key))
         return values.get(key);
      return null;
   }

}
