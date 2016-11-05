package org.proactive;

import java.lang.reflect.Constructor;

public interface ReactComponentFactoryConfiguration
{
   public abstract Constructor<? extends ReactComponent> getImplementingClass(String key);
   public abstract void registerComponent(String key, Constructor<? extends ReactComponent> constructor);
}

