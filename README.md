Proactive
=========

A Prolog implementation of the React concept. Documentation is available in the doc directory.

## TL;DR
Write UI applications in Prolog using an XML-like syntax:
```
render(State, Props, MyApp):-
    findall(Field,
            render_some_field(State, Props, Field),
            SeveralFields),
    {|jsx(MyApp)||
        <Panel>
          <Label label="Hello from Prolog!"/>
          {SeveralFields}
          <Button label="Submit" onClick={click_handler}/>
        </Panel>|}.

on_click(Event, State, Props, NewState):-
   ...

```


How it works
------------
There are three trees that you have to be mindful of when trying to understand what is happening.

## The Virtual DOM
First, there's the virtual DOM. This exists in Prolog as element(Tag, Attributes, Children) terms. There's also a special case list/1 which can appear anywhere, and is simply an artifact of the way JSX converts variable-length elements into vDOM at compile-time. Your render/3 predicate must return one of these terms. It barely leaks into Java - for the moat part it stays inside the WAM

## The Java DOM
This is a partial implementation of the HTML DOM, and is implemented by ReactComponent. Each component supports DOM-like methods such as getChildNodes(), getParentNode() and replaceChild().

## The Swing Tree
The visible components are created out of the Java DOM on demand via getAWTComponent().

### Diagram
```

      <Panel>  . . . . . . . . . . . . . . . . (org.proactive.ui.Panel) . . . . . . . . . . . . . . . . . . . . . . . . . . . . . {JPanel}
        /\                                               /\                                                                         / \
       /  \_______                                      /  \_________________                                                      /   \
      /           \                                    /                     \                                                    /     \
     |             \                                  /                       \                                                  /       \
<MyComponent/>   <Button/>             (org.proactive.ReactWidget)    (org.proactive.ui.Button)                                 |     {JButton}
     .                                               .                                                                          |
     . (defined as)                                  . (refers to)                                                              | (contains)
     .                                               .                                                                          |
  <Panel/>                                (org.proactive.ui.Panel)                                                           {JPanel}
     |                                               |                                                                          |
     |                                               |                                                                         / \
    / \____                                         / \______________________                                                 /   \____
   /       \                                       /                         \                                               /         \
<Label/>  <Field/>                     (org.proactive.ui.Label)    (org.proactive.ui.Field)                              {JLabel}  {JTextField}
```
Note that the trees do not correspond 1-1 with each other. Specifically, things get a bit complicated when dealing with the user-defined components.

---++ proactivejs
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
