var ReactComponent = require('./react_component');

function Broken(name)
{
    ReactComponent.call(this);
    var node = document.createElement("div");
    node.textContent = "Broken-" + name;
    this.setDOMNode(node);
}

Broken.prototype = new ReactComponent;

module.exports = Broken;
