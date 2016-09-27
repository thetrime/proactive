"use strict";

var ReactComponent = require('./react_component');
var Prolog = require('proscript');

function ComboItem()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("option"));
}

ComboItem.prototype = new ReactComponent;

ComboItem.prototype.setProperties = function(t)
{
   ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
        this.domNode.textContent = Prolog._portray(t.label);
    if (t.value !== undefined)
    {
        this.domNode.value = t.value;
        this.value = t.value;
    }
}

ComboItem.prototype.getValue = function()
{
    return this.value;
}

ComboItem.prototype.setSelected = function(t)
{
    this.domNode.selected = t;
}

module.exports = ComboItem;
