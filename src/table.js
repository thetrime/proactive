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
    console.log("Hello?");
    this.table.appendChild(t.getDOMNode());
}

module.exports = Table;
