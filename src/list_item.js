var ReactComponent = require('./react_component');

function ListItem()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("li"));
}

ListItem.prototype = new ReactComponent;

ListItem.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
        this.domNode.textContent = t.label;
    if (t.key !== undefined)
        this.key = t.key;
}

module.exports = ListItem;
