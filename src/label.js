var ReactComponent = require('./react_component');

function Label()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("div"));
}
Label.prototype = new ReactComponent;

Label.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
        this.domNode.textContent = t.label;
}

module.exports = Label;
