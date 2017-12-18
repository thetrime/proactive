var ReactComponent = require('./react_component');

function TableFooter()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("tr"));
}
TableFooter.prototype = new ReactComponent;

TableFooter.prototype.appendChild = function(t)
{
    var cell = document.createElement("td");
    cell.appendChild(t.getDOMNode());
    this.domNode.appendChild(cell);
    t.setParent(this);
}

TableFooter.prototype.removeChild = function(t)
{
    var index = this.children.indexOf(t);
    this.children.splice(index, 1);
    t.setParent(null);
    this.domNode.removeChild(this.domNode.childNodes[index]);
    if (this.parent != null)
        this.parent.markDirty();
}

TableFooter.prototype.insertBefore = function(t, s)
{
    var cell = document.createElement("td");
    cell.appendChild(t.domNode);
    var index = this.children.indexOf(s);
    this.domNode.insertBefore(cell, s.getDOMNode().parentNode);
    t.setParent(this);
    if (this.parent != null)
        this.parent.markDirty();
}

TableFooter.prototype.replaceChild = function(n, o)
{
    var cell = document.createElement("td");
    cell.appendChild(t.domNode);
    var index = this.children.indexOf(s);
    this.domNode.replaceChild(cell, o.getDOMNode().parentNode);
    n.setParent(this);
    o.setParent(null);
    if (this.parent != null)
        this.parent.markDirty();
}
module.exports = TableFooter;
