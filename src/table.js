var ReactComponent = require('./react_component');

function Table()
{
    ReactComponent.call(this);
    this.table = document.createElement("table");
    // table does not play nice with flexbox. Instead put it inside a div which will and set it to 100% width
    this.table.style.width = "100%";
    var node = document.createElement("div");
    node.appendChild(this.table);
    this.setDOMNode(node);
}
Table.prototype = new ReactComponent;

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
