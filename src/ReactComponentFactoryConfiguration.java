package org.proactive;

import java.lang.reflect.Constructor;

public interface ReactComponentFactoryConfiguration
{
   public abstract Constructor<? extends ReactComponent> getImplementingClass(String key);
}

