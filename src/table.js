var ReactComponent = require('./react_component');
var TableHeader = require('./table_header');

function Table()
{
    ReactComponent.call(this);
    this.table = document.createElement("table");
    // table does not play nice with flexbox :(
    this.table.className = "react_table scrollpane";
    this.baseClassName = "table_container"
    this.container = document.createElement("div");
    this.container.appendChild(this.table);
    this.setDOMNode(this.container);
}
Table.prototype = new ReactComponent;

//Table.prototype.getStyle = function()
//{
//    return "table_container";
//}

Table.prototype.appendChild = function(t)
{
    this.table.appendChild(t.getDOMNode());
    t.setParent(this);
}

Table.prototype.insertBefore = function(t, s)
{
    this.table.insertBefore(t.getDOMNode(), s.getDOMNode());
    t.setParent(this);
}

Table.prototype.replaceChild = function(n, o)
{
    this.table.replaceChild(n.getDOMNode(), o.getDOMNode());
    n.setParent(this);
    o.setParent(null);
}

Table.prototype.removeChild = function(t)
{
    this.table.removeChild(t.getDOMNode());
    t.setParent(null);
}


module.exports = Table;
