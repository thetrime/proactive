package org.proactive.ui;

import javax.swing.JEditorPane;
import java.util.List;
import java.util.HashMap;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;

public class EditorPane extends ReactLeafComponent 
{
   JEditorPane editorPane = new JEditorPane();
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      if (properties.containsKey("fill"))
         fill = properties.get("fill").asFill();
   }

   public Component getAWTComponent()
   {
      return editorPane;
   }
}
