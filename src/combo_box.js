"use strict";

var Prolog = require('../lib/proscript2/build/proscript.js');
var ReactComponent = require('./react_component');


function ComboBox()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("select"));
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
        // FIXME: Implement
    }
}

function blurHandler(event)
{
    this.getOwnerDocument().triggerEvent(this.changeHandler, ReactComponent.serialize({value: new Prolog.AtomTerm(this.value)}));
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
    this.domNode.onBlur = blurHandler.bind(this);
}

ComboBox.prototype.setValue = function(t)
{
    this.value = t;
    // FIXME: need to actually select the right child here!
}

module.exports = ComboBox;
