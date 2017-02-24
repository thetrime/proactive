package org.proactive.ui;

public class ProactiveConstraints
{
   public enum Fill {HORIZONTAL, VERTICAL, BOTH, NONE};
   public Fill fill = Fill.NONE;
   public int index = -1;
   public ProactiveConstraints(Fill fill, int index)
   {
      this.fill = fill;
      this.index = index;
   }
}