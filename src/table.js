var ReactComponent = require('./react_component');
var TableHeader = require('./table_header');

function Table()
{
    ReactComponent.call(this);
    this.table = document.createElement("table");
    // table does not play nice with flexbox. Instead put it inside a div which will and set it to 100% width
    this.table.className = "react_table";
    this.table.style["border-spacing"] = 0;
    var node = document.createElement("div");
    this.baseClassName = "table_container"
    this.header_container = document.createElement("div");
    //this.header_container.className = "table_header_container";
    this.header_container.appendChild(this.table);
    node.appendChild(this.header_container);
    this.setDOMNode(node);
}
Table.prototype = new ReactComponent;

Table.prototype.getStyle = function()
{
    return "table_container";
}

Table.prototype.appendChild = function(t)
{
    this.table.appendChild(t.getDOMNode());
    if (t instanceof TableHeader)
        this.header_container.className = "table_header_container";
    t.setParent(this);
}

Table.prototype.insertBefore = function(t, s)
{
    if (t instanceof TableHeader)
        this.header_container.className = "table_header_container";
    this.table.insertBefore(t.getDOMNode(), s.getDOMNode());
    t.setParent(this);
}

Table.prototype.replaceChild = function(n, o)
{
    if (o instanceof TableHeader)
        this.header_container.className = "";
    if (n instanceof TableHeader)
        this.header_container.className = "table_header_container";
    this.table.replaceChild(n.getDOMNode(), o.getDOMNode());
    n.setParent(this);
    o.setParent(null);
}


Table.prototype.removeChild = function(t)
{
    if (t instanceof TableHeader)
        this.header_container.className = "";
    this.table.removeChild(t.getDOMNode());
    t.setParent(null);
}


module.exports = Table;
