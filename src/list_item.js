var ReactComponent = require('./react_component');

function ListItem()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("li"));
    this.key = null;
}

ListItem.prototype = new ReactComponent;

ListItem.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
        this.domNode.textContent = Prolog._portray(t.label);
    if (t.key !== undefined)
    {
        if (this.key != null)
            Prolog._free_local(this.key);
        if (ReactComponent.isNull(t.key))
            this.key = null;
        else
            this.key = Prolog._make_local(t.key);
    }
}

module.exports = ListItem;
