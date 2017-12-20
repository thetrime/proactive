var ReactComponent = require('./react_component');

var default_weight = 1;

function TableHeader()
{
    ReactComponent.call(this);
    this.tr = document.createElement("tr");
    this.setDOMNode(this.tr);
}

TableHeader.prototype = new ReactComponent;


TableHeader.prototype.appendChild = function(t)
{
    var cell = document.createElement("th");
    cell.className = "react_table_header_cell";
    cell.appendChild(t.domNode);
    this.getDOMNode().appendChild(cell);
    t.setParent(this);
    if (this.parent != null)
    {
        this.parent.column_count++;
        this.parent.markDirty();
    }
}


TableHeader.prototype.removeChild = function(t)
{
    var index = this.children.indexOf(t);
    t.setParent(null);
    this.tr.removeChild(this.tr.childNodes[index]);
    if (this.parent != null)
    {
        this.parent.column_count--;
        this.parent.markDirty();
    }
}

TableHeader.prototype.insertBefore = function(t, s)
{
    var cell = document.createElement("th");
    cell.className = "react_table_header_cell";
    cell.appendChild(t.domNode);
    var index = this.children.indexOf(s);
    this.tr.insertBefore(cell, s.getDOMNode().parentNode);
    t.setParent(this);
    if (this.parent != null)
    {
        this.parent.column_count++;
        this.parent.markDirty();
    }
}

TableHeader.prototype.replaceChild = function(n, o)
{
    var cell = document.createElement("th");
    cell.className = "react_table_header_cell";
    cell.appendChild(t.domNode);
    var index = this.children.indexOf(s);
    this.domNode.replaceChild(cell, o.getDOMNode().parentNode);
    n.setParent(this);
    o.setParent(null);
    if (this.parent != null)
    {
        this.parent.column_count++;
        this.parent.markDirty();
    }
}

module.exports = TableHeader;
