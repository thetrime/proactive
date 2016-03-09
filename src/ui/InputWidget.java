package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import java.awt.Component;

public interface InputWidget
{
   public Component getAWTComponent();
   public void setValue(PrologObject value);
   public Object getValue();
   public void setChangeListener(InputWidgetListener listener);
}
