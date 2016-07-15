var ReactComponent = require('./react_component');

function List()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("ul"));
}

List.prototype = new ReactComponent;

List.prototype.getDOMNode = function()
{
    return this.domNode;
}

module.exports = List;
