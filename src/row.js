var ReactComponent = require('./react_component');
var Prolog = require('proscript');
var Constants = require('./constants.js');

function Row()
{
    ReactComponent.call(this);
    var row = document.createElement("tr");
    this.baseClassName = "react_table_row";
    this.setDOMNode(row);
    this.elements = [];
}

Row.prototype = new ReactComponent;
Row.prototype.appendChild = function(t)
{
    var cell = document.createElement("td");
    cell.appendChild(t.getDOMNode());
    this.domNode.appendChild(cell);
    this.elements.push(cell);
    this.children.push(t);
    t.setParent(this);
    if (this.parent != null)
        this.parent.markDirty();
}

Row.prototype.insertBefore = function(t, s)
{
    var cell = document.createElement("td");
    cell.appendChild(t.getDOMNode());
    var index = this.children.indexOf(s);
    this.children.splice(index, 0, s);
    this.domNode.insertBefore(cell, s.getDOMNode().parentNode);
    this.elements.splice(index, 0, cell);
    t.setParent(this);
    if (this.parent != null)
        this.parent.markDirty();
}

Row.prototype.replaceChild = function(n, o)
{
    var cell = document.createElement("td");
    cell.appendChild(t.getDOMNode());
    var index = this.children.indexOf(o);
    this.children[index] = n;
    this.domNode.replaceChild(cell, o.getDOMNode().parentNode.parentNode);
    this.elements[index] = cell;
    n.setParent(this);
    o.setParent(null);
    if (this.parent != null)
        this.parent.markDirty();
}

Row.prototype.removeChild = function(t)
{
    var index = this.children.indexOf(t);
    this.elements.splice(index, 1);
    this.children.splice(index, 1);
    this.domNode.removeChild(t.getDOMNode().parentNode);
    t.setParent(null);
    if (this.parent != null)
        this.parent.markDirty();
}


Row.prototype.setProperties = function(t)
{
   ReactComponent.prototype.setProperties.call(this, t);
    if (t.onDblClick !== undefined)
        this.setDblClickHandler(t.onDblClick);
}

Row.prototype.setDblClickHandler = function(value)
{
    if (this.dblClickHandler != null)
        Prolog._free_local(this.dblClickHandler);
    if (ReactComponent.isNull(value))
    {
        if (this.domNode.onDblClick !== undefined)
            this.domNode.onDblClick = undefined;
        return;
    }
    this.dblClickHandler = Prolog._make_local(value);
    this.domNode.ondblclick = dblClickHandler.bind(this);
}

Row.prototype.getElements = function()
{
    return this.elements;
}

function dblClickHandler(event)
{
    this.getOwnerDocument().triggerEvent(this.dblClickHandler, Constants.emptyListAtom, function() {});
}


module.exports = Row;
