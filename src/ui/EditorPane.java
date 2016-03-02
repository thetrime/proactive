package org.proactive.ui;

import javax.swing.JEditorPane;
import java.util.List;
import java.awt.Component;
import org.proactive.vdom.PrologNode;
import org.proactive.prolog.PrologContext;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactLeafComponent;

public class EditorPane extends ReactLeafComponent 
{
   JEditorPane editorPane = new JEditorPane();
   public EditorPane(PrologNode n, PrologContext context)
   {
      super(context);
      /* This is if the child is the label.
        Node child = n.getFirstChild();
        if (child != null && child instanceof Text)
        setText(((Text)child).getWholeText());
      */
   }
   public void setProperty(String name, PrologObject value)
   {
      if (name.equals("fill"))
         fill = value.asFill();
   }
   public Component getAWTComponent()
   {
      return editorPane;
   }
}