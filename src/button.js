var ReactComponent = require('./react_component');

function Button()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("button"));
}

Button.prototype = new ReactComponent;

Button.prototype.setProperties = function(t)
{
   ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
        this.domNode.textContent = t.label;
}

module.exports = Button;
