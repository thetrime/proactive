# proactivejs
Make web pages directly in Prolog

Let's combine reactive-prolog and proscript. What could possibly go wrong?

Note that at the moment things are in a state of flux. I'm working on an iOS native version and trying to combine all the projects together. If successful, I may end up including the proactive project in here and renaming this one to proactive instead

---++ To Do List:
---+++ ObjC version:
   * Review the license. Currently it's not possible to distribute anything
      * Investigate replacing libgmp with something BSD-licensed as required (ie with a compile-time flag)
   * Implement the following types:
      * List
      * ListItem
      * Table
      * Title
      * TableHeader
      * Row
      * TabbedPane
      * Tab
      * ComboBox
      * ComboItem
   * Implement non-atomic handlers
   * Implement scroll attributes
   * Implement releaseBlob
   * Implement init_widget/3, update_widget/4 and test sub-widgets
   * Implement insert_before/3
