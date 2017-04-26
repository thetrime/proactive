Components
==========

The following components and attributes are supported in the JS and Swing clients. (The ObjC client lacks support for at least some of these)

Layout
======
All visible components support the following attributes:
   * fill: one of 'none', 'horizontal', 'vertical' or 'both' - if the parent component has excess space, this controls how it will be allocated. Children with 'none' as their fill value will take up only the minimum space needed to render, while those with 'horizontal' will take up a share of any excess horizontal space, those with 'vertical' will take up a share of any excess vertical space, and those with 'both' will take up both horizontal and vertical space.
   * weight: An integer used to allow specification of relative importance of components. This is currently used for the Table component to determine how wide each of the columns should be relative to one another

Proactive is based (roughly) on the Flexbox layout system. A component which has children has two axes: major and minor, which correspond to the vertical and horizontal axes, with the mapping between them governed by an attribute called "layout". If the layout is 'horizontal' then the major axis is the horizontal axis, and the minor axis is the vertical. If the layout is 'vertical' then the major axis is the vertical axis and the minor axis is the horizontal.


Panel
-----
The Panel is the basic layout unit. In Swing it's represented as a JPanel, and in JS as a div. It supports the following attributes:
   * scroll: One of 'none', 'horizontal', 'vertical' or 'both'. Controls whether the component will scroll (or simply truncate its children) if it is overfull
   * label: An atom which will be displayed in the border of the component. Useful for grouping together similar components under a single banner
   * background: An atom specifying the background style for the component. For JS this is the style.background. Not supported in Swing.
   * layout: One of 'horizontal' or 'vertical'. Controls the major axis for which child objects will be laid out.
   * align-children: One of 'start', 'center', 'end', or 'stretch'. Controls how the children will be aligned in the minor axis. 'start' will arrange them at the left (or top), 'center' in the middle, 'end' at the right (or bottom) and 'stretch' will cause them to take up the entire minor axis
   * justify-content: One of 'start', 'end', 'center', 'space-between' or 'space-around'. Controls the alignment along the major axis if none of the components have a fill in the major direction. See align-children for a description of the first three. 'space-between' allocates free space between the components (so the first and last will be flush with the container edge) and 'space-around' allocates equal space before and after each component (so there will be a gap between the edge of the container and the first/last component).

Grid
----
The Grid is a layout component with slightly more complex rules for layout. It has these attributes:
   * weights: A list of integers (currently only zero and non-zero are differentiated in the JS client). A value of 0 indicates that the column should be as big as needed to display the longest cell in it, whereas 1 indicates that the column should be allocated a share of any extra space. This can be used to align a group of components in a tabular layout.
   * padding: A value (in pixels or em, css-style) which corresponds to inter-cell width padding. Padding is only added on the left and right of cells, and is omitted from before the first and after the last element in a row.

Button
------
The Button is a UI button component which the user can interact with. It supports the following attributes:
   * label: Any object. This will be printed on the label using write/1.
   * disabled: If boolean(true) the button will be disabled. If boolean(false) (or absent) then the button will be enabled.
   * onClick: An event handler to be fired when the button is clicked. The event data will be empty.

Label
-----
The Label is a component which displays text. It supports the following attributes:
   * label: Any object. This will be printed on the label using write/1. If this is an atom containing HTML it will be displayed as HTML.
   * title: The tooltip. By default if no tooltip is supplied, the label will be used
   * for (JS only): An atom used to associate the label with another component. This can be used to focus a field if the associated label is clicked
   * align (JS only): An atom used to specify the text-align style attribute of the text
   * overflow (JS only): If 'ellipsis', then when the label doesn't have space to display in entirety, the label is truncated slightly further to display "..." at the end

List
----
The List is a component for display a listing of items. It has no attributes of its own (see ListItem). It is not yet complete enough to be useful except for displaying data.

ListItem
--------
The ListItem is a object to display in a List. It has these attributes:
   * label: Any object. This will be printed on the item using write/1
   * key: Any object to associate with the list item so you can identify it later
   * selected (Swing only): Whether the list item is selected

Table
-----
The Table is a component to display tabular data. It can contain 0 or 1 TableHeader components and 0..n Row components. It supports the following attributes:
   * renderContextMenu (Swing only, unstable API): A renderer to generate a context menu when a cell is right-clicked (or long-pressed, in theory)

TableHeader
-----------
The TableHeader is the component displayed at the top of a table (typically including column headers). It can contain any number of components; each one will specify a column. The weight attribute of these objects governs the relative widths of the columns. It has no attributes of its own.

TableFooter
-----------
The TableHeader is the component displayed at the bottom of a table (typically something like a total or summary table). It should contain Row objects (you can have a footer with multiple rows, but a header must have only a single row, so for brevity you add the cells directly to the TableHeader).


Row
---
The Row is a container for row data in a table. A row can contain arbitrarily many components, but you may get weird results if the rows in a Table do not all contain the same number of children. It supports the following attributes:
   * onDblClick: A handler to run when the row is double clicked. The event will be empty.

Field
-----
The Field is a single-line input component. It supports the following attributes:
   * type: One of 'text', 'password', 'radio' or 'checkbox'. A 'password' field is a 'text' field where the input is obscured.
   * value: An *atom* describing the value the Field currently represents. For boolean fields, allowed values are 'true' and 'false'.
   * disabled: If boolean(true) the Field will be disabled. If boolean(false) (or absent) then the field will be enabled.
   * onContextMenu (Swing only): A handler to fire when the context (ie popup) menu should be displayed. The event object will contain the current value in the field associated with the key 'value'.
   * renderContextMenu (Swing only, unstable API): A renderer to generate a context menu when the Field is right-clicked (or long-pressed, in theory)
   * onBlur: A handler to fire when the Field loses focus. The event object will contain the current value in the field associated with the key 'value'.
   * onChange: A handler to fire when the Field would change its value because of user interaction. The event object will contain the current value in the Field associated with the key 'value'.
   * verifyValue: A goal to fire when the Field would lose focus. If the goal fails, then focus transfer will be vetoed. The event object will contain the current value in the Field associated with the key 'value'.
   * maxWidth (JS only): The maximum width of the Field.
   * align: One of 'left' or 'right' to govern whether the text should be aligned to the left or the right of the Field
   * title (JS only): An atom to appear in the tooltip

TextArea
--------
The TextArea is a multi-line text input component. It supports the following attributes:
   * value: An *atom* describing the value the TextArea currently represents.
   * disabled: If boolean(true) the TextArea will be disabled. If boolean(false) (or absent) then the TextArea will be enabled.
   * onContextMenu (Swing only): A handler to fire when the context (ie popup) menu should be displayed. The event object will contain the current value in the TextArea associated with the key 'value'.
   * renderContextMenu (Swing only, unstable API): A renderer to generate a context menu when the TextArea is right-clicked (or long-pressed, in theory)
   * onBlur: A handler to fire when the TextArea loses focus. The event object will contain the current value in the TextArea associated with the key 'value'.
   * onChange: A handler to fire when the TextArea would change its value because of user interaction. The event object will contain the current value in the TextArea associated with the key 'value'.
   * verifyValue: A goal to fire when the TextArea would lose focus. If the goal fails, then focus transfer will be vetoed. The event object will contain the current value in the TextArea associated with the key 'value'.
   * maxWidth (JS only): The maximum width of the TextArea.
   * align: One of 'left' or 'right' to govern whether the text should be aligned to the left or the right of the TextArea
   * title (JS only): An atom to appear in the tooltip

ComboBox
--------
The ComboBox is a component which allows the user to select a single value from a list of options. It supports the following attributes:
   * value: The value of the currently selected ComboItem
   * onBlur: A handler to fire when the ComboBox loses focus. The event object will contain the current value of the ComboItem currently selected in the ComboBox, associated with the key 'value'.
   * onChange: A handler to fire when the ComboBox would change its value because of user interaction. The event object will contain the current value of the ComboItem currently selected in the ComboBox, associated with the key 'value'.
   * disabled: If boolean(true) the ComboBox will be disabled. If boolean(false) (or absent) then the ComboBox will be enabled.
   * maxWidth (JS only): The maximum width of the ComboBox.

ComboItem
---------
The ComboItem is an item which may appear inside a ComboBox. It supports the following attributes:
   * label: Any object. This will be printed on the ComboItem using write/1
   * value: Any object. This will appear in events generated from the ComboBox to identify this ComboItem as the one which is selected

Frame
-----
The Frame is a component designed to interrupt the visual flow. In JS it is implemented as a div which appears over the top of other components. In Swing, it is implemented as a separate Window. Currently the Swing and JS implementations are quite different; these may be merged in the future. It supports the following attributes:
   * scroll (JS only): One of 'none', 'horizontal', 'vertical' or 'both'. Controls whether the component will scroll (or simply truncate its children) if it is overfull
   * onClose: (Swing only): A handler to fire when the window is closed by the user
   * z_index (JS only): An integer determining the Z-order of the frame. Frames with higher values for z_index will appear above those with a lower one

Image
-----
The Image is displays an image. It supports the following attributes:
   * width: An integer determining the width of the image (otherwise the image itself will determine the width)
   * height: An integer determining the height of the image (otherwise the image itself will determine the height)
   * src: An atom specifying a URL where the image can be obtained


TabbedPane
----------
The TabbedPane is a layout component that can contain 0 or more Tab objects. It has no attributes of its own

Tab
---
The Tab is a mutually-exclusive panel which is displayed inside a TabbedPane. At most one Tab can be visible in a TabbedPane at any given time. It has the following attributes:
   * label: Any object. This will be printed on the button which will be used to select this Tab on the TabbedPane.