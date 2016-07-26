"use strict";

var Prolog = require('../lib/proscript2/src/core.js');
var ReactComponent = require('./react_component');
var ComboItem = require('./combo_item');


function ComboBox()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("select"));
    this.domNode.onchange = function()
    {
        var proposedIndex = this.domNode.selectedIndex;
        this.domNode.selectedIndex = this.currentIndex;
        if (this.changeHandler != null)
        {
            this.getOwnerDocument().triggerEvent(this.changeHandler, ReactComponent.serialize({value: this.children[proposedIndex].getValue()}), function() {});
        }
    }.bind(this);
}

ComboBox.prototype = new ReactComponent;

ComboBox.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.value !== undefined)
    {
        this.setValue(t.value);
    }
    if (t.onBlur !== undefined)
    {
        this.setBlurHandler(t.onBlur);
    }
    if (t.disabled !== undefined)
    {
        this.domNode.disabled = ReactComponent.booleanValue(t.disabled);
    }
    if (t.onChange !== undefined)
    {
        this.setChangeHandler(t.onChange);
    }
}

function blurHandler(event)
{
    console.log("Triggering blur");
    this.getOwnerDocument().triggerEvent(this.blurHandler, ReactComponent.serialize({value: this.children[this.currentIndex].getValue()}), function() {});
}

ComboBox.prototype.setBlurHandler = function(value)
{
    if (ReactComponent.isNull(value))
    {
        this.blurHandler = null;
        this.domNode.onBlur = undefined;
        return;
    }
    this.blurHandler = value;
    this.domNode.onblur = blurHandler.bind(this);
    console.log("Installed a blurhandler");
}

ComboBox.prototype.setChangeHandler = function(value)
{
    if (ReactComponent.isNull(value))
        this.changeHandler = null;
    else
        this.changeHandler = value;
}

ComboBox.prototype.appendChild = function(t)
{
    ReactComponent.prototype.appendChild.call(this, t);
    if (t instanceof ComboItem && t.getValue() == this.value)
        this.selectCurrentIndex();
    else
        t.setSelected(false);
}

ComboBox.prototype.replaceChild = function(n, o)
{
    ReactComponent.prototype.replaceChild.call(this, n, o);
    if (n instanceof ComboItem && n.getValue() == this.value)
        this.selectCurrentIndex();
    else
        n.setSelected(false);
}

ComboBox.prototype.insertBefore = function(n, s)
{
    ReactComponent.prototype.insertBefore.call(this, n, s);
    if (n instanceof ComboItem && n.getValue() == this.value)
        this.selectCurrentIndex();
    else
        n.setSelected(false);
}

ComboBox.prototype.setValue = function(t)
{
    this.value = t;
    this.selectCurrentIndex();
}

ComboBox.prototype.selectCurrentIndex = function()
{
    for (var i = 0; i < this.children.length; i++)
    {
        if (this.children[i] instanceof ComboItem && this.children[i].getValue() == this.value)
        {
            this.currentIndex = i;
            this.children[i].setSelected(true);
            break;
        }
    }
}

module.exports = ComboBox;
