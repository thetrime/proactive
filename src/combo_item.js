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
        this.domNode.value = Prolog._portray(t.value);
        if (this.value != null)
            Prolog._free_local(this.value);
        this.value = Prolog._make_local(t.value);
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

ComboItem.prototype.freeComponent = function(vNode)
{
    if (this.value != null)
        Prolog._free_local(this.value);
    ReactComponent.prototype.freeComponent.call(this, vNode); //ie super.freeComponent(vNode)
}

module.exports = ComboItem;
