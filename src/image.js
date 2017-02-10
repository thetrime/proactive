var ReactComponent = require('./react_component');
var Prolog = require('proscript');
var Constants = require('./constants.js');

function Image()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("img"));
}
Image.prototype = new ReactComponent;

Image.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.src !== undefined)
    {
        this.domNode.src = Prolog._atom_chars(t.src);
    }
    if (t.width !== undefined)
    {
        this.domNode.width = ReactComponent.numericValueOr(t.width, 0);
    }
    if (t.height !== undefined)
    {
        this.domNode.height = ReactComponent.numericValueOr(t.height, 0);
    }

}

module.exports = Image;
