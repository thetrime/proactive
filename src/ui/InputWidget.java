package org.proactive.ui;

import org.proactive.prolog.PrologObject;
import java.awt.Component;

public interface InputWidget
{
   public Component getAWTComponent();
   public void setValue(PrologObject value);
   public Object getValue();
   public void setDisabled(boolean disabled);
   public void setChangeListener(InputWidgetListener listener);
   public void setVerifier(InputWidgetVerifier verifier);
   public void setAlignment(int alignment);
}
