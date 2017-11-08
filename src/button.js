"use strict";

var Constants = require('./constants.js');
var ReactComponent = require('./react_component');
var Prolog = require('proscript');

function Button()
{
    ReactComponent.call(this);
    this.setDOMNode(document.createElement("button"));
    this.imageNode = null;
}

function clickHandler(event)
{
    if (this.clickHandler != null)
        this.getOwnerDocument().triggerEvent(this.clickHandler, Constants.emptyListAtom, function() {});
}


Button.prototype = new ReactComponent;

Button.prototype.setProperties = function(t)
{
   ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
        this.domNode.textContent = Prolog._portray(t.label);
    if (t.image !== undefined)
    {
        if (ReactComponent.isNull(t.image) && this.imageNode != null)
            this.domNode.removeChild(imageNode);
        else
        {
            this.imageNode = document.createElement("img");
            this.imageNode.src = Prolog._portray(t.image);
            this.domNode.appendChild(this.imageNode);
        }
    }
    if (t.onClick !== undefined)
        this.setClickHandler(t.onClick);
    if (t.disabled !== undefined)
        this.domNode.disabled = ReactComponent.booleanValue(t.disabled);
}

Button.prototype.setClickHandler = function(value)
{
    if (this.clickHandler != null)
    {
        Prolog._free_local(this.clickHandler);
    }
    if (ReactComponent.isNull(value))
    {
        if (this.domNode.onClick !== undefined)
            this.domNode.onClick = undefined;
        this.clickHandler = null;
        return;
    }
    this.clickHandler = Prolog._make_local(value);
    this.domNode.onclick = clickHandler.bind(this);
}

module.exports = Button;
