"use strict";

var Constants = require('./constants.js');
var ReactComponent = require('./react_component');
var Prolog = require('proscript');

function MenuItem(name)
{
    ReactComponent.call(this);
    var node = document.createElement("div");
    this.baseClassName = "proactive_menu_item"
    this.clickHandler = null;
    this.setDOMNode(node);
}

function clickHandler(event)
{
    this.getOwnerDocument().triggerEvent(this.clickHandler, Constants.emptyListAtom, function() {});
}

MenuItem.prototype = new ReactComponent;

MenuItem.prototype.setProperties = function(t)
{
   ReactComponent.prototype.setProperties.call(this, t);
    if (t.label !== undefined)
        this.domNode.textContent = Prolog._portray(t.label);
    if (t.onClick !== undefined)
        this.setClickHandler(t.onClick);
    if (t.disabled !== undefined)
        this.domNode.disabled = ReactComponent.booleanValue(t.disabled);
}

MenuItem.prototype.setClickHandler = function(value)
{
    if (this.clickHandler != null)
        Prolog._free_local(this.clickHandler);
    if (ReactComponent.isNull(value))
    {
        if (this.domNode.onClick !== undefined)
            this.domNode.onClick = undefined;
        return;
    }
    this.clickHandler = Prolog._make_local(value);
    this.domNode.onclick = clickHandler.bind(this);
}

// FIXME: Clean up the clickHandler when the menu is destroyed!

module.exports = MenuItem;
