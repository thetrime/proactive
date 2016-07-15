var ReactComponent = require('./react_component');

function TableHeader()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("tr"));
}

TableHeader.prototype = new ReactComponent;

TableHeader.prototype.appendChild = function(t)
{
    var cell = document.createElement("th");
    cell.appendChild(t.domNode);
    this.domNode.appendChild(cell);
}

module.exports = TableHeader;
