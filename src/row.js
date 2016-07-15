var ReactComponent = require('./react_component');

function Row()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("tr"));
}

Row.prototype = new ReactComponent;
Row.prototype.appendChild = function(t)
{
    var cell = document.createElement("td");
    cell.appendChild(t.getDOMNode());
    this.domNode.appendChild(cell);
}

module.exports = Row;
