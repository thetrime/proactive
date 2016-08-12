var ReactComponent = require('./react_component');
var Prolog = require('../lib/proscript2/build/proscript.js');

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
        this.domNode.textContent = Prolog._portray(t.label);
}

module.exports = Label;
