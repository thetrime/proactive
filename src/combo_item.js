"use strict";

var Prolog = require('../lib/proscript2/src/core.js');
var ReactComponent = require('./react_component');


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
        this.domNode.textContent = t.label;
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
