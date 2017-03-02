package org.proactive.ui;

public class ProactiveConstraints
{
   public enum Fill {HORIZONTAL, VERTICAL, BOTH, NONE};
   public enum Justification {START, CENTER, END, SPACE_BETWEEN, SPACE_AROUND};
   public enum Alignment {START, END, CENTER, BASELINE, STRETCH, AUTO};

   public Fill fill = Fill.NONE;
   public int index = -1;
   public Alignment selfAlignment;

   public ProactiveConstraints(Fill fill, Alignment selfAlignment, int index)
   {
      this.fill = fill;
      this.selfAlignment = selfAlignment;
      this.index = index;
   }
}
