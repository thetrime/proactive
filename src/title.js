var ReactComponent = require('./react_component');
var Prolog = require('proscript');

function Title()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("h3"));
}
Title.prototype = new ReactComponent;

Title.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
    {
        var v = Prolog._portray(t.label);
        if (v.startsWith("<html>"))
            this.domNode.innerHTML = v;
        else
            this.domNode.textContent = v;
    }
}

module.exports = Title;
