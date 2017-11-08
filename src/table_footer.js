var ReactComponent = require('./react_component');

function TableFooter()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("tr"));
    this.elements = [];
}
TableFooter.prototype = new ReactComponent;

TableFooter.prototype.appendChild = function(t)
{
    var cell = document.createElement("td");
    cell.appendChild(t.getDOMNode());
    this.domNode.appendChild(cell);
    this.elements.push(cell);
    t.setParent(this);
}

TableFooter.prototype.getElements = function()
{
    return this.elements;
}

module.exports = TableFooter;
