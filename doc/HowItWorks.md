How it works
------------
There are three trees that you have to be mindful of when trying to understand what is happening.

## The Virtual DOM
First, there's the virtual DOM. This exists in Prolog as element(Tag, Attributes, Children) terms. There's also a special case list/1 which can appear anywhere, and is simply an artifact of the way JSX converts variable-length elements into vDOM at compile-time. Your render/3 predicate must return one of these terms. It barely leaks into Java - for the moat part it stays inside the WAM

## The DOM
This is a partial implementation of the HTML DOM, and is implemented by ReactComponent. Each component supports DOM-like methods such as getChildNodes(), getParentNode() and replaceChild().

## The Component Tree
The visible components are created out of the DOM on demand via (eg) getAWTComponent().

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

## Other Clients
This information pertains to the Swing client. There are at least three other clients (swipl, JS, ios) which are described in their own file in the doc directory. The principle is similar for all of them, but the implementation varies between them.